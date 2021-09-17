{-# LANGUAGE DeriveDataTypeable #-}

module BehavioralModel where

import Data.Data
import Data.Generics.Uniplate.Data
import qualified Data.ByteString as BS

type Ident = BS.ByteString
type PCFormula = BS.ByteString
type ModelName = BS.ByteString
type MessageName = BS.ByteString
type FragmentName = BS.ByteString
type FeatureName = BS.ByteString
type SDName = BS.ByteString

data Model =
  Model { mName             :: BS.ByteString
        , mActivityDiagram  :: ActivityDiagram
        , mSequenceDiagrams :: SequenceDiagrams
        } deriving (Eq, Ord, Show, Data, Typeable)


data ActivityDiagram =
  ActivityDiagram { adElements    :: [ActivityDiagramElement]
                  , adTransitions :: [Transition]
                  } deriving (Eq, Ord, Show, Data, Typeable)

data ActivityDiagramElement =
  ActivityDiagramElement { adElementName  :: BS.ByteString
                         , adElementType  :: BS.ByteString
                         , adElementRepBy :: Maybe BS.ByteString
                         } deriving (Eq, Ord, Show, Data, Typeable)

data Transition =
  Transition { transitionName        :: BS.ByteString
             , transitionProbability :: Double
             , transitionSource      :: BS.ByteString
             , transitionTarget      :: BS.ByteString
             } deriving (Eq, Ord, Show, Data, Typeable)

data SequenceDiagrams =
  SequenceDiagrams { sdsSequenceDiagrams :: [SequenceDiagram]
                   , sdsLifelines        :: [Lifeline]
                   , sdsFragments        :: [Fragment]
                   } deriving (Eq, Ord, Show, Data, Typeable)

data SequenceDiagram =
  SequenceDiagram { sdName       :: SDName
                  , sdGuard      :: PCFormula     -- presence condition
                  , sdComponents :: [Either Message Fragment]
                  } deriving (Eq, Ord, Show, Data, Typeable)

data Message =
  Message { messageName        :: MessageName
          , messageProbability :: Double
          , messageSource      :: BS.ByteString       -- lifeline
          , messageTarget      :: BS.ByteString       -- lifeline
          , messageType        :: MessageType
          } deriving (Eq, Ord, Show, Data, Typeable)

data MessageType = Synchronous | Asynchronous
  deriving (Eq, Ord, Show, Data, Typeable)

data Lifeline =
  Lifeline { lifelineName        :: BS.ByteString
           , lifelineReliability :: Double
           } deriving (Eq, Ord, Show, Data, Typeable)

data Fragment =
  Fragment { fragmentName :: FragmentName
           , fragmentType :: FragmentType
           , fragmentRepBy :: Maybe BS.ByteString  -- seqDiagName
           } deriving (Eq, Ord, Show, Data, Typeable)

data FragmentType = Optional
  deriving (Eq, Ord, Show, Data, Typeable)
