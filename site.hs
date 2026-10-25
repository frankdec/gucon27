--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
import              Data.Monoid (mappend)
import              Data.Functor.Contravariant
import qualified    Data.ByteString.Lazy            as BL
import qualified    Data.Aeson                      as Aeson
import              Hakyll
import              Hakyll.Core.Provider
import              Hakyll.Core.Compiler
import              Hakyll.Core.Compiler.Internal
import qualified    GHC.IO.Encoding                 as Encoding
import              System.FilePath.Posix
import              Talk


--------------------------------------------------------------------------------
main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    hakyllWith defaultConfiguration $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "templates/*" $ compile templateBodyCompiler

        match ("**/*.markdown" .||. "*.markdown") $ do
            route cleanRoute
            compile $ mdCompiler defaultContext

        match "talks/*.yaml" $ do
            compile parseTalkCompiler 

        match "index.html" $ do
            route idRoute
            compile $ do
                talks <- loadAll @Talk "talks/*.yaml"
                let ctx = listField "talks" talkContext (pure talks) `mappend`
                          defaultContext 
                getResourceBody
                    >>= applyAsTemplate ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

--------------------------------------------------------------------------------
-- configuration :: Configuration
-- configuration = defaultConfiguration{ providerDirectory = "site" }

mdCompiler :: Context String -> Compiler (Item String)
mdCompiler ctx =
    pandocCompiler
    >>= applyAsTemplate ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls

cleanRoute :: Routes
cleanRoute = customRoute $
    (\(p, _) -> p </> "index" <.> "html") . splitExtension . toFilePath

talkContext :: Context Talk
talkContext =
    mconcat [ field "talkTitle" (pure . title . itemBody)
            , field "presenter" (pure . presenter . itemBody)
            , field "abstract" (pure . abstract . itemBody)
            ]

parseTalkCompiler :: Compiler (Item Talk)
parseTalkCompiler = do
    identifier  <- getUnderlying
    provider    <- compilerProvider <$> compilerAsk
    body        <- unsafeCompiler $ BL.readFile (resourceFilePath provider identifier)
    talk        <- parseTalkM (BL.toStrict body)
    makeItem talk
