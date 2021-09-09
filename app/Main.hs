{-# LANGUAGE OverloadedStrings #-}
module Main where

import XML (modelFromNode)
import BehavioralModel
import Xeno.DOM (parse)
import Text.Pretty.Simple (pPrint)
import System.Environment
import Data.Generics
import Control.Monad.Except
import Data.Maybe (catMaybes)
import Data.Either (isLeft)

import qualified Data.List as L

import qualified Data.ByteString as BS
import qualified Data.Set as S

findFragments :: Model -> [Fragment]
findFragments = everything (++) ([] `mkQ` query)
  where query m@Fragment {} = [m]

findFragmentsSDs :: Model -> [(Fragment, Maybe SequenceDiagram)]
findFragmentsSDs m = zip fragments sds
  where fragments = findFragments m
        sds = map (`getCorrespondingSD` m) fragments

getCorrespondingSD :: Fragment -> Model -> Maybe SequenceDiagram
getCorrespondingSD f = everything orElse (Nothing `mkQ` findSD f)
  where findSD :: Fragment -> SequenceDiagram -> Maybe SequenceDiagram
        findSD f sd = do
          fragmentSDName <- fragmentRepBy f
          if sdName sd == fragmentSDName
            then Just sd
            else Nothing

verifyFragments :: Model -> Model -> (S.Set Fragment, S.Set Fragment)
verifyFragments m1 m2 = (addedFragments, removedFragments)
  where fragmentSet1 = S.fromList $ findFragments m1
        fragmentSet2 = S.fromList $ findFragments m2
        addedFragments = S.difference fragmentSet2 fragmentSet1
        removedFragments = S.difference fragmentSet1 fragmentSet2

-- TODO try to eliminate map _ . filter _
getSDMessages :: SequenceDiagram -> [Message]
getSDMessages = map (\(Left m) -> m) . filter isLeft . sdComponents

verifyMessages :: Model -> Model -> [(Fragment, S.Set Message, S.Set Message)]
verifyMessages m1 m2 =
  let fragments = findFragments m1
      z = zip fragments $ map (\frag -> verifyFragmentMessages frag m1 m2) fragments
   in untup z

verifyFragmentMessages :: Fragment -> Model -> Model -> (S.Set Message, S.Set Message)
verifyFragmentMessages frag m1 m2 = (addedMessages, removedMessages)
  where sd1 = getCorrespondingSD frag m1
        sd2 = getCorrespondingSD frag m2
        messageSet1 = maybe S.empty (S.fromList . getSDMessages) sd1
        messageSet2 = maybe S.empty (S.fromList . getSDMessages) sd2
        addedMessages = S.difference messageSet2 messageSet1
        removedMessages = S.difference messageSet1 messageSet2

verifyPresenceConditions :: Model -> Model -> [(Fragment, PCFormula, PCFormula)]
verifyPresenceConditions m1 m2 = catMaybes something
  where fragments = findFragments m1
        sds1 = map (`getCorrespondingSD` m1) fragments
        pcs1 = (sdGuard <$>) <$> sds1
        sds2 = map (`getCorrespondingSD` m2) fragments
        pcs2 = (sdGuard <$>) <$> sds2
        something = zipWith3 (\fragment pc1 pc2 ->
                                do
                                  pc1' <- pc1
                                  pc2' <- pc2
                                  if pc1' /= pc2'
                                     then pure (fragment, pc1', pc2')
                                     else Nothing)
                             fragments pcs1 pcs2

untup :: [(a, (b, c))] -> [(a, b, c)]
untup = map (\(x, (y, z)) -> (x, y, z))

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
    ["-f", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let (addedFragments, removedFragments) = verifyFragments m1 m2
      putStrLn "Fragments added: "
      pPrint addedFragments
      putStrLn "\nFragments removed: "
      pPrint removedFragments
    ["-m", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let changes = verifyMessages m1 m2
      mapM_ (\(frag, addedMessages, removedMessages) -> do
        if S.null addedMessages && S.null removedMessages
           then putStr ""
           else do
            pPrint frag
            putStrLn "\tMessages added: "
            pPrint addedMessages
            putStrLn "\nMessages removed: "
            pPrint removedMessages
            putStrLn ""
        ) changes
    ["-pc", f1, f2] -> do
      m1 <- readModel f1
      m2 <- readModel f2
      let changes = verifyPresenceConditions m1 m2
      putStrLn "Presence Condition Changes"
      pPrint changes
    _ -> putStrLn "Please provide an XML model file as argument"


readModel :: String -> IO Model
readModel filename = do
  xmlText <- BS.readFile filename
  case parse xmlText of
    Left err -> undefined
    Right node ->
      case modelFromNode node of
        Left err -> undefined
        Right model -> pure model

