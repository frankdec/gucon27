{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
import              GHC.Generics
import              Data.Monoid (mappend)
import              Data.Functor.Contravariant
import qualified    Data.ByteString.Lazy            as BL
import qualified    Data.Aeson                      as Aeson
import              Data.Maybe (fromMaybe)
import              Hakyll
import              Hakyll.Core.Provider
import              Hakyll.Core.Compiler
import              Hakyll.Core.Compiler.Internal
import qualified    GHC.IO.Encoding                 as Encoding
import              Text.Pandoc.Options
import              System.FilePath.Posix


myContext :: Context String
myContext = mconcat
    [ field "presenter" (content "presenter")
    , field "moderator" (content "moderator")
    , field "startTime" (content "startTime")
    , field "endTime" (content "endTime")
    , defaultContext
    ]
    where
        content :: String -> Item String -> Compiler String
        content key item = do
            metadata <- getMetadata (itemIdentifier item)
            pure $ fromMaybe "not found" $ lookupString key metadata

main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    hakyll $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "templates/*" $ compile templateBodyCompiler

        match "*.markdown" $ do
            route cleanRoute
            compile $ mdCompiler

        match "talks/*.markdown" $ do
            route cleanRoute
            compile $ talkCompiler

        match "events.html" $ do
            route cleanRoute
            compile $ do
               talks <- loadAll "talks/*.markdown"
               let ctx = listField "talks" myContext (pure talks) `mappend` myContext
               getResourceBody
                    >>= applyAsTemplate ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                getResourceBody
                    >>= applyAsTemplate myContext
                    >>= loadAndApplyTemplate "templates/default.html" myContext
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
        >>= loadAndApplyTemplate "templates/talk.html" myContext
        >>= loadAndApplyTemplate "templates/default.html" myContext
        >>= relativizeUrls

cleanRoute :: Routes
cleanRoute = customRoute $
    (\(p, _) -> p </> "index" <.> "html") . splitExtension . toFilePath

katexCompiler :: Compiler (Item String)
katexCompiler = pandocCompilerWith def writerOptions
    where
        writerOptions :: WriterOptions
        writerOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = KaTeX "" }
