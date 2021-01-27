{-# LANGUAGE DeriveDataTypeable #-}

module BehavioralModel where

import Data.Data
import Data.Generics.Uniplate.Data
import qualified Data.ByteString as BS

data Model =
  Model { mName             :: BS.ByteString
        , mActivityDiagram  :: ActivityDiagram
        , mSequenceDiagrams :: SequenceDiagrams
        } deriving (Eq, Show, Data, Typeable)


data ActivityDiagram =
  ActivityDiagram { adElements    :: [ActivityDiagramElement]
                  , adTransitions :: [Transition]
                  } deriving (Eq, Show, Data, Typeable)

data ActivityDiagramElement =
  ActivityDiagramElement { adElementName  :: BS.ByteString
                         , adElementType  :: BS.ByteString
                         , adElementRepBy :: Maybe BS.ByteString
                         } deriving (Eq, Show, Data, Typeable)

data Transition =
  Transition { transitionName        :: BS.ByteString
             , transitionProbability :: Double
             , transitionSource      :: BS.ByteString
             , transitionTarget      :: BS.ByteString
             } deriving (Eq, Show, Data, Typeable)

data SequenceDiagrams =
  SequenceDiagrams { sdsSequenceDiagrams :: [SequenceDiagram]
                   , sdsLifelines        :: [Lifeline]
                   , sdsFragments        :: [Fragment]
                   } deriving (Eq, Show, Data, Typeable)

data SequenceDiagram =
  SequenceDiagram { sdName       :: BS.ByteString
                  , sdGuard      :: BS.ByteString  -- presence condition
                  , sdComponents :: [Either Message Fragment]
                  } deriving (Eq, Show, Data, Typeable)

data Message =
  Message { messageName        :: BS.ByteString
          , messageProbability :: Double
          , messageSource      :: BS.ByteString       -- lifeline
          , messageTarget      :: BS.ByteString       -- lifeline
          , messageType        :: MessageType
          } deriving (Eq, Show, Data, Typeable)

data MessageType = Synchronous | Asynchronous
  deriving (Eq, Show, Data, Typeable)

data Lifeline =
  Lifeline { lifelineName        :: BS.ByteString
           , lifelineReliability :: Double
           } deriving (Eq, Show, Data, Typeable)

data Fragment =
  Fragment { fragmentName :: BS.ByteString
           , fragmentType :: FragmentType
           , fragmentRepBy :: Maybe BS.ByteString  -- seqDiagName
           } deriving (Eq, Show, Data, Typeable)

data FragmentType = Optional
  deriving (Eq, Show, Data, Typeable)
