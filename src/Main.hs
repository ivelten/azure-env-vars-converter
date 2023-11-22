{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import GHC.Generics
import Data.Aeson
import Data.Aeson.Casing
import Data.Map as Map hiding (foldr)
import Data.ByteString.Lazy.Char8 hiding (foldr, putStrLn)
import Prelude hiding (readFile, writeFile)

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

importSetting :: AzureConfig -> Map String String -> Map String String
importSetting cfg = Map.insert (name cfg) (value cfg)

convertSettings :: [AzureConfig] -> LocalSettings
convertSettings cfgs =
  LocalSettings { isEncrypted = False
                , values      = foldr importSetting Map.empty cfgs }

main :: IO ()
main = do
  putStrLn "Reading input file."
  input <- readFile "input.json"
  let azureConfig = eitherDecode input :: Either String [AzureConfig]
  let localSettings = case azureConfig of
        Left err   -> error err
        Right cfgs -> convertSettings cfgs
  let output = encode localSettings
  putStrLn "Writing output file."
  writeFile "local.settings.json" output
  putStrLn "Done."
