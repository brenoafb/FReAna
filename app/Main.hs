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
    ["-fr", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let (addedFragments, removedFragments) = verifyFragments m1 m2
      putStrLn "Fragments added: "
      pPrint addedFragments
      putStrLn "\nFragments removed: "
      pPrint removedFragments
    ["-pc", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let changes = verifyPresenceConditions m1 m2
      putStrLn "Presence Condition Changes"
      pPrint changes
    ["-m", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let changes = verifyMessages m1 m2
      mapM_ (\(frag, addedMessages, removedMessages) -> do
        if S.null addedMessages && S.null removedMessages
           then putStr ""
           else do
            pPrint frag
            putStrLn "Messages added: "
            pPrint addedMessages
            putStrLn "\nMessages removed: "
            pPrint removedMessages
            putStrLn ""
        ) changes
    ["-fe", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let addedFeatures = verifyFeatures m1 m2
      putStrLn "Features added: "
      pPrint addedFeatures
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

