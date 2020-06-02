{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as Map
import           Data.Text           (intercalate)
import           Data.Text.IO        (putStrLn)
import           Lib
import           Prelude             hiding (putStrLn)

main :: IO ()
main = do
  command <- getCommand
  yaml_ <- loadYaml
  case yaml_ of
    Left e -> print e
    Right yaml -> do
      let script = joinScripts $ getScriptsByCommand command yaml
      putStrLn $ case script of
        "" -> "Command not found\n----------Commands List----------\n" <> intercalate "\n" (Map.keys $ scripts yaml)
        _ -> "scripter exec \"" <> script <> "\""
      exec script
