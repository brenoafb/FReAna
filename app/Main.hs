{-# LANGUAGE OverloadedStrings #-}
module Main where

import XML (modelFromNode)
import Xeno.DOM (parse)
import Text.Pretty.Simple (pPrint)
import System.Environment
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      xmlText <- BS.readFile filename
      case parse xmlText of
        Left err -> print err
        Right node ->
          case modelFromNode node of
            Left err -> print err
            Right model -> pPrint model
    _ -> putStrLn "Please provide an XML model file as argument"

