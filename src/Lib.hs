{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, intercalate, pack, unpack)
import           Data.Yaml           (FromJSON, ParseException,
                                      decodeFileEither)
import           Prelude             hiding (putStrLn)
import           System.Environment  (getArgs)
import           Turtle              hiding (FilePath)

-- Yamlファイルの型
newtype Yaml =
  Yaml
    { scripts :: Map.HashMap Text [Text]
    }
  deriving (Show, Read, Eq)
$(deriveJSON defaultOptions ''Yaml)

-- シェルスクリプト実行
exec :: (MonadIO m) => Text -> m ()
exec cmd = do
  x <- shell cmd empty
  case x of
    ExitSuccess   -> return ()
    ExitFailure n -> void (die (cmd <> " failed with exit code: " <> repr n))

-- スクリプトを && でつなげる
joinScripts :: [Text] -> Text
joinScripts = intercalate " && "

-- yamlファイルからスクリプトを取得
getScriptsByCommand :: Text -> Yaml -> [Text]
getScriptsByCommand command yaml = fromMaybe [] (Map.lookup command (scripts yaml))

-- コマンドの読み取り
getCommand :: IO Text
getCommand = do
  args <- getArgs
  pure $
    pack $
    case args of
      [x] -> x
      _   -> ""

-- yamlの読み込み
loadYaml :: IO (Either ParseException Yaml)
loadYaml = decodeFileEither "scripts.yml"
