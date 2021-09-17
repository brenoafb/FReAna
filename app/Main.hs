{-# LANGUAGE OverloadedStrings #-}
module Main where

import XML (modelFromNode)
import BehavioralModel
import Xeno.DOM (parse)
import Text.Pretty.Simple (pPrint)
import System.Environment
import Control.Monad.Except

import qualified Data.List as L

import qualified Data.ByteString as BS
import qualified Data.Set as S

import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> readModel filename >>= pPrint
    ["-f", filename] -> do
      model <- readModel filename
      let fragments = L.sort $ findFragments model
      pPrint fragments
    ["-fs", f] -> do
      m <- readModel f
      let fragmentSDs = findFragmentsSDs m
      pPrint fragmentSDs
    [f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      pPrint $ verify m1 m2
    _ -> putStrLn "Please provide an XML model file as argument"


readModel :: String -> IO Model
readModel filename = do
  xmlText <- BS.readFile filename
  case parse xmlText of
    Left err -> print err >> undefined
    Right node ->
      case modelFromNode node of
        Left err -> print err >> undefined
        Right model -> pure model

