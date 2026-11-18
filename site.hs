{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (liftM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import qualified GHC.IO.Encoding as Encoding
import Hakyll
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Provider
import System.FilePath.Posix
import Text.Pandoc.Options

conferenceName :: String
conferenceName = "Logos"

conferenceNameYear :: String
conferenceNameYear = conferenceName ++ " 2027"

myContext' :: Context String
myContext' =
  mconcat
    [ field "presenter" (content "presenter")
    , field "moderator" (content "moderator")
    , field "startTime" (content "startTime")
    , field "endTime" (content "endTime")
    , constField "conferenceName" conferenceName
    , constField "conferenceNameYear" conferenceNameYear
    , defaultContext
    ]
 where
  content :: String -> Item String -> Compiler String
  content key item = do
    metadata <- getMetadata (itemIdentifier item)
    pure $ fromMaybe "not found" $ lookupString key metadata

myContext :: Context String
myContext =
  let events = loadAllSnapshots "events/*.markdown" "content"
        >>= byStartTime
   in listField "events" myContext' events
        `mappend` myContext'

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8
  hakyll $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "*.markdown" $ do
      route cleanRoute
      compile $ mdCompiler

    match "events/*.markdown" $ do
      route cleanRoute
      compile $ eventCompiler

    match "about.html" $ do
      route cleanRoute
      compile $ do
        getResourceBody
          >>= applyAsTemplate myContext
          >>= loadAndApplyTemplate "templates/default.html" myContext
          >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        -- Replace the `title` metadata with `conferenceNameYear`.
        let ctx =
              constField "title" conferenceNameYear
                `mappend` myContext
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

mdCompiler :: Compiler (Item String)
mdCompiler =
  pandocCompiler
    >>= applyAsTemplate myContext
    >>= loadAndApplyTemplate "templates/default.html" myContext
    >>= relativizeUrls

eventCompiler :: Compiler (Item String)
eventCompiler =
  katexCompiler
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/talk.html" myContext
    >>= loadAndApplyTemplate "templates/default.html" myContext
    >>= relativizeUrls


getStartTime :: Identifier -> Compiler String
getStartTime id = getMetadataField' id "startTime" 

byStartTime :: [Item a] -> Compiler [Item a]
byStartTime = sortByM $ getStartTime . itemIdentifier
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

cleanRoute :: Routes
cleanRoute =
  customRoute $
    (\(p, _) -> p </> "index" <.> "html") . splitExtension . toFilePath

katexCompiler :: Compiler (Item String)
katexCompiler = pandocCompilerWith def writerOptions
 where
  writerOptions :: WriterOptions
  writerOptions = defaultHakyllWriterOptions{writerHTMLMathMethod = KaTeX ""}
