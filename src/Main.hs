module Main (main) where

import Prelude hiding (readFile, writeFile)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Data.Map as Map hiding (foldr)
import Data.ByteString.Lazy.Char8 hiding (foldr, putStrLn)

data AzureConfig = AzureConfig { name  :: String
                               , value :: String
                               , slotSetting :: Bool
                               } deriving (Eq, Show, Generic)

instance FromJSON AzureConfig where
  parseJSON = genericParseJSON $ aesonDrop 0 camelCase

data LocalSettings = LocalSettings { isEncrypted :: Bool
                                   , values      :: Map String String
                                   } deriving (Eq, Show, Generic)

instance ToJSON LocalSettings where
  toJSON = genericToJSON $ aesonDrop 0 pascalCase

addConfigToMap :: AzureConfig -> Map String String -> Map String String
addConfigToMap cfg = Map.insert (name cfg) (value cfg)

convertConfigsToMap :: [AzureConfig] -> Map String String
convertConfigsToMap = foldr addConfigToMap Map.empty

convertSettings :: [AzureConfig] -> LocalSettings
convertSettings cfgs =
  LocalSettings { isEncrypted = False
                , values      = convertConfigsToMap cfgs }

decodeConfigs :: ByteString -> Either String [AzureConfig]
decodeConfigs = eitherDecode

main :: IO ()
main = do
  putStrLn "Reading input file."
  inputBytes <- readFile "input.json"
  let azureConfig = decodeConfigs inputBytes
  let localSettings = case azureConfig of
        Left err   -> error err
        Right cfgs -> convertSettings cfgs
  let output = encode localSettings
  putStrLn "Writing output file."
  writeFile "local.settings.json" output
  putStrLn "Done."
