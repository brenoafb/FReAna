{-# LANGUAGE OverloadedStrings #-}
module Lib where

import BehavioralModel

import Data.Maybe (catMaybes)
import Data.Either (isRight, isLeft)
import Data.Generics

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.ByteString as BS

data Change
  = AddMessage  FragmentName MessageName
  | RemMessage  FragmentName MessageName
  | ChangePC    FragmentName PCFormula PCFormula -- fragment, old formula, new formula
  | AddFragment FragmentName
  | RemFragment FragmentName
  | AddFeature  FeatureName
  deriving (Eq, Show)

verify :: Model -> Model -> [Change]
verify m1 m2 =
  verifyFragments m1 m2
  ++ verifyMessages m1 m2
  ++ verifyPresenceConditions m1 m2
  ++ verifyFeatures m1 m2

findFragments :: Model -> [Fragment]
findFragments = L.nub . everything (++) ([] `mkQ` query)
  where query m@Fragment {} = [m]

findSDs :: Model -> [SequenceDiagram]
findSDs = L.nub . everything (++) ([] `mkQ` query)
  where query m@SequenceDiagram {} = [m]

findFragmentsSDs :: Model -> [(Fragment, Maybe SequenceDiagram)]
findFragmentsSDs m = zip fragments sds
  where fragments = findFragments m
        sds = map (`getCorrespondingSD` m) fragments

getSDByName :: Model -> SDName -> Maybe SequenceDiagram
getSDByName m queryName =
  case filter (\sd -> sdName sd == queryName) $ findSDs m of
    [x] -> Just x
    _   -> Nothing

getCorrespondingSD :: Fragment -> Model -> Maybe SequenceDiagram
getCorrespondingSD f = everything orElse (Nothing `mkQ` findSD f)
  where findSD :: Fragment -> SequenceDiagram -> Maybe SequenceDiagram
        findSD f sd = do
          fragmentSDName <- fragmentRepBy f
          if sdName sd == fragmentSDName
            then Just sd
            else Nothing

verifyFragments :: Model -> Model -> [Change]
verifyFragments m1 m2 = changes
  where fragmentSet1 = S.fromList $ findFragments m1
        fragmentSet2 = S.fromList $ findFragments m2
        addedFragments = S.toList $ S.difference fragmentSet2 fragmentSet1
        removedFragments = S.toList $ S.difference fragmentSet1 fragmentSet2
        changes = map (AddFragment . fragmentName) addedFragments
               ++ map (RemFragment . fragmentName) removedFragments

getSDMessages :: SequenceDiagram -> [Message]
getSDMessages = map (\(Left m) -> m) . filter isLeft . sdComponents

getSDFragments :: SequenceDiagram -> [Fragment]
getSDFragments = map (\(Right m) -> m) . filter isRight . sdComponents

verifyMessages :: Model -> Model -> [Change]
verifyMessages m1 m2 =
  concatMap (\f -> verifyFragmentMessages f m1 m2) (findFragments m1)
  ++ concatMap (\sd -> verifySDMessages sd m1 m2)  (findSDs m1)

verifyFragmentMessages :: Fragment -> Model -> Model -> [Change]
verifyFragmentMessages frag m1 m2 = changes
  where sd1 = getCorrespondingSD frag m1
        sd2 = getCorrespondingSD frag m2
        messageSet1 = maybe S.empty (S.fromList . getSDMessages) sd1
        messageSet2 = maybe S.empty (S.fromList . getSDMessages) sd2
        addedMessages = S.toList $ S.difference messageSet2 messageSet1
        removedMessages = S.toList $ S.difference messageSet1 messageSet2
        changes = map (AddMessage (maybe "FIXME" sdName sd1) . messageName) addedMessages
               ++ map (RemMessage (maybe "FIXME" sdName sd2) . messageName) removedMessages -- FIXME

verifySDMessages :: SequenceDiagram -> Model -> Model -> [Change]
verifySDMessages sd m1 m2 =
  case getSDByName m2 (sdName sd) of
    Nothing -> error $ "SD not found: " <> show (sdName sd)  -- FIXME
    Just sd' ->
      let messageSet1 = S.fromList $ getSDMessages sd
          messageSet2 = S.fromList $ getSDMessages sd'
          addedMessages = S.toList $ S.difference messageSet2 messageSet1
          removedMessages = S.toList $ S.difference messageSet1 messageSet2
       in map (AddMessage (sdName sd) . messageName) addedMessages
       ++ map (RemMessage (sdName sd) . messageName) removedMessages

verifyPresenceConditions :: Model -> Model -> [Change]
verifyPresenceConditions m1 m2 = catMaybes changes
  where fragments = findFragments m1
        sds1 = map (`getCorrespondingSD` m1) fragments
        pcs1 = (sdGuard <$>) <$> sds1
        sds2 = map (`getCorrespondingSD` m2) fragments
        pcs2 = (sdGuard <$>) <$> sds2
        changes = zipWith3 (\fragment pc1 pc2 ->
                                do
                                  pc1' <- pc1
                                  pc2' <- pc2
                                  if pc1' /= pc2'
                                     then pure $ ChangePC (fragmentName fragment) pc1' pc2'
                                     else Nothing)
                             fragments pcs1 pcs2

verifyFeatures :: Model -> Model -> [Change]
verifyFeatures m1 m2 = map AddFeature . S.toList $ S.difference pcs2 pcs1
  where pcs1 = S.fromList . map sdGuard $ getSequenceDiagrams m1
        pcs2 = S.fromList . map sdGuard $ getSequenceDiagrams m2

getSequenceDiagrams :: Model -> [SequenceDiagram]
getSequenceDiagrams = sdsSequenceDiagrams . mSequenceDiagrams

untup :: [(a, (b, c))] -> [(a, b, c)]
untup = map (\(x, (y, z)) -> (x, y, z))
