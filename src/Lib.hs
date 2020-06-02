{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
  ( libMain
  ) where

import           Data.Aeson.TH       (defaultOptions, deriveJSON)
import           Data.Foldable       (foldl')
import           Data.HashMap.Strict (HashMap, keys, lookup)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, intercalate, pack, unpack)
import           Data.Yaml           (FromJSON, ParseException,
                                      decodeFileEither)
import           System.Environment  (getArgs)
import           Turtle              hiding (FilePath)

-- Yamlファイルの型
newtype Yaml =
  Yaml
    { scripts :: HashMap Text [Text]
    }
  deriving (Show, Read, Eq)

$(deriveJSON defaultOptions ''Yaml)

libMain :: IO ()
libMain = do
  command <- getCommand
  yaml_ <- loadYaml
  case yaml_ of
    Left e -> print e
    Right yaml -> do
      let script = joinScripts $ getScriptsByCommand command yaml
      putStrLn $
        unpack
          (if script /= ""
             then "scripter exec \"" <> script <> "\""
             else "Command not found\n----------Commands List----------\n" <> intercalate "\n" (keys $ scripts yaml))
      exec script

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
      []  -> ""
      [x] -> x
      _   -> ""

-- yamlの読み込み
loadYaml :: IO (Either ParseException Yaml)
loadYaml = decodeFileEither "scripts.yml"
