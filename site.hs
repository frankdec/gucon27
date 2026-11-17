{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import GHC.Generics
import qualified GHC.IO.Encoding as Encoding
import Hakyll
import Hakyll.Core.Compiler
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Provider
import System.FilePath.Posix
import Text.Pandoc.Options

conferenceName :: String
conferenceName = "LOGO"

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
  let talks = loadAllSnapshots "talks/*.markdown" "content"
   in listField "talks" myContext' talks
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

    match "talks/*.markdown" $ do
      route cleanRoute
      compile $ talkCompiler

    match ("events.html" .||. "about.html") $ do
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

talkCompiler :: Compiler (Item String)
talkCompiler =
  katexCompiler
    >>= saveSnapshot "content"
    >>= loadAndApplyTemplate "templates/talk.html" myContext
    >>= loadAndApplyTemplate "templates/default.html" myContext
    >>= relativizeUrls

cleanRoute :: Routes
cleanRoute =
  customRoute $
    (\(p, _) -> p </> "index" <.> "html") . splitExtension . toFilePath

katexCompiler :: Compiler (Item String)
katexCompiler = pandocCompilerWith def writerOptions
 where
  writerOptions :: WriterOptions
  writerOptions = defaultHakyllWriterOptions{writerHTMLMathMethod = KaTeX ""}
