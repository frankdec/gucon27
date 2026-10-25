{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Talk where
import              Hakyll
import              GHC.Generics
import              Data.Binary
import qualified    Data.ByteString         as BS
import qualified    Data.ByteString.Lazy    as BL
import qualified    Data.Aeson              as Aeson
import qualified    Data.Yaml               as Yaml

data Talk = Talk
    { title     :: String
    , presenter :: String
    , abstract  :: String
    } deriving (Eq, Show, Generic)

instance Aeson.FromJSON Talk
instance Aeson.ToJSON Talk

instance Binary Talk where
    put Talk{..} = do
        put title
        put presenter
        put abstract
    get = Talk <$> get <*> get <*> get 

instance Writable Talk where
    write path Item{..} =
        BL.writeFile path $ Aeson.encode itemBody

parseTalkM :: MonadFail m => BS.ByteString -> m Talk
parseTalkM body =
    case Yaml.decodeEither' body of
        Left err -> fail $ "unable to parse talk file: " <> show err
        Right content -> pure content
