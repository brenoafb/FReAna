{-# LANGUAGE OverloadedStrings #-}

module XML where

import BehavioralModel
import Xeno.DOM
import Xeno.Types
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.Map as M

type Error = BS.ByteString

node `childrenNamed` cname =
  filter (\n -> name n == cname) $ children node

node `childNamed` cname =
  case node `childrenNamed` cname of
    []  -> Left $ "No children named " <> cname
    [x] -> Right x
    _   -> Left $ "More than one child named " <> cname

nodeLookup :: Node -> BS.ByteString -> Either Error BS.ByteString
nodeLookup node attr =
  case lookup attr (attributes node) of
    Nothing -> Left $ "No attribute named " <> attr
    Just x  -> Right x

-- | Read a model from a XML file node
-- For an example XML file, view "bsn-reduced.xml"
modelFromNode :: Node -> Either Error Model
modelFromNode node
  | name node /= "SplBehavioralModel" = Left "Invalid XML (not SplBehavioralModel)"
  | otherwise = do
    name      <- getModelName node
    ad        <- node `childNamed` "ActivityDiagram"  >>= adFromNode
    sds       <- node `childNamed` "SequenceDiagrams" >>= sdsFromNode
    pure Model { mName = name
               , mActivityDiagram  = ad
               , mSequenceDiagrams = sds
               }

getModelName :: Node -> Either Error BS.ByteString
getModelName node =
  case lookup "name" $ attributes node of
    Nothing -> Left "Error getting model name (no \"name\" attribute)"
    Just x  -> pure x

-- | Read an ActivityDiagram from a respective node
-- Example XML node containing an activity diagram
-- ```xml
--  <ActivityDiagram name="AD_SPL_0">
--    <Elements>
--      <ActivityDiagramElement name="Start node" type="StartNode" />
--      <ActivityDiagramElement name="Capture" type="Activity">
--        <RepresentedBy seqDiagName="Capture" />
--      </ActivityDiagramElement>
--    </Elements>
--    <Transitions>
--      <Transition name="Capture" probability="1.0" source="Start node" target="Capture" />
--    </Transitions>
--  </ActivityDiagram>
-- ```
adFromNode :: Node -> Either Error ActivityDiagram
adFromNode node
  | name node /= "ActivityDiagram" = Left "Invalid Node (not \"ActivityDiagram\")"
  | otherwise = do
      elementsNode <- node `childNamed` "Elements"
      adElements <-  mapM adElementFromNode (elementsNode `childrenNamed` "ActivityDiagramElement")
      transitionsNode <- node `childNamed` "Transitions"
      adTransitions <- mapM adTransitionFromNode (transitionsNode `childrenNamed` "Transition")
      pure ActivityDiagram { adElements    = adElements
                           , adTransitions = adTransitions
                           }

-- | Read an ActivityDiagramElement from a respective node
-- Example XML node containing an ActivityDiagramElement
-- ```xml
-- <ActivityDiagramElement name="Start node" type="StartNode" />
-- ```
-- An ActivityDiagramElement can alternatively contain a "RepresentedBy" child
-- ```xml
-- <ActivityDiagramElement name="Capture" type="Activity">
--   <RepresentedBy seqDiagName="Capture" />
-- </ActivityDiagramElement>
-- ```
adElementFromNode :: Node -> Either Error ActivityDiagramElement
adElementFromNode node = do
  adName <- nodeLookup node "name"
  typ    <- nodeLookup node "type"
  case children node of
    [] -> pure $ ActivityDiagramElement adName typ Nothing
    [x] | name x == "RepresentedBy" -> do
            repBy <- nodeLookup x "seqDiagName"
            pure $ ActivityDiagramElement adName typ (Just repBy)
    _ -> Left "Invalid ActivityDiagramElement node"

-- | Read a Transition from a respective node
-- Example XML node containing a Transition
-- ```xml
-- <Transition name="Capture" probability="1.0" source="Start node" target="Capture" />
-- ```
adTransitionFromNode :: Node -> Either Error Transition
adTransitionFromNode node = do
  tName <- nodeLookup node "name"
  tProb <- nodeLookup node "probability" >>= read'
  tSrc  <- nodeLookup node "source"
  tTrgt <- nodeLookup node "target"
  pure $ Transition { transitionName = tName
                    , transitionProbability = tProb
                    , transitionSource = tSrc
                    , transitionTarget = tTrgt
                    }

-- | Read a SequenceDiagrams object from a respective node
-- Example XML node containing a SequenceDiagrams object
-- A SequenceDiagrams object contains a collection of
-- SequenceDiagrams, Lifelines, and Fragments.
-- Example XML node containing a SequenceDiagram
-- ```xml
-- <SequenceDiagrams>
--   <SequenceDiagram guard="true" name="QoSChange">
--     <Message name="" probability="0.999" source="Mock lifeline" target="Lifeline_0" type="asynchronous" />
--   </SequenceDiagram>
--   <Lifelines>
--     <Lifeline name="Mock lifeline" reliability="0.999" />
--   </Lifelines>
--   <Fragments>
--     <Fragment name="n6" type="optional">
--       <RepresentedBy seqDiagName="n6" />
--     </Fragment>
--   </Fragments>
-- </SequenceDiagrams>
-- ```
sdsFromNode :: Node -> Either Error SequenceDiagrams
sdsFromNode node = do
  sdsSequenceDiagrams <- mapM sdFromNode
                              (node `childrenNamed` "SequenceDiagram")
  lifelinesNode       <- node `childNamed` "Lifelines"
  sdsLifelines        <- mapM lifelineFromNode
                              (lifelinesNode `childrenNamed` "Lifeline")
  fragmentsNode       <- node `childNamed` "Fragments"
  sdsFragments        <- mapM fragmentFromNode
                              (fragmentsNode `childrenNamed` "Fragment")
  pure SequenceDiagrams { sdsSequenceDiagrams = sdsSequenceDiagrams
                        , sdsLifelines        = sdsLifelines
                        , sdsFragments        = sdsFragments
                        }

-- | Read a SequenceDiagram from a respective node.
-- ```xml
-- <SequenceDiagram guard="true" name="QoSChange">
--   <Message name="" probability="0.999" source="Mock lifeline" target="Lifeline_0" type="asynchronous" />
-- </SequenceDiagram>
-- ```
sdFromNode :: Node -> Either Error SequenceDiagram
sdFromNode node = do
  sdName      <- nodeLookup node "name"
  sdGuard     <- nodeLookup node "guard"
  sdMessages  <- mapM messageFromNode $ node `childrenNamed` "Message"
  sdFragments <- mapM fragmentFromNode $ node `childrenNamed` "Fragments"
  pure SequenceDiagram { sdName      = sdName
                       , sdGuard     = sdGuard
                       , sdMessages  = sdMessages
                       , sdFragments = sdFragments
                       }

-- adFromNode :: Node -> Either Error ActivityDiagram
-- adFromNode node
--   | name node /= "ActivityDiagram" = Left "Invalid Node (not \"ActivityDiagram\")"
--   | otherwise = do
--       elementsNode <- node `childNamed` "Elements"
--       adElements <-  mapM adElementFromNode (elementsNode `childrenNamed` "ActivityDiagramElement")
--       transitionsNode <- node `childNamed` "Transitions"
--       adTransitions <- mapM adTransitionFromNode (transitionsNode `childrenNamed` "Transition")
--       pure ActivityDiagram { adElements    = adElements
--                            , adTransitions = adTransitions
--                            }

-- | Read a Message from a respective node.
-- ```xml
-- <Message name="" probability="0.999" source="Mock lifeline" target="Lifeline_0" type="asynchronous" />
-- ```
messageFromNode :: Node -> Either Error Message
messageFromNode node = do
  mName <- nodeLookup node "name"
  mProb <- nodeLookup node "probability" >>= read'
  mSrc  <- nodeLookup node "source"
  mTrgt <- nodeLookup node "target"
  mType <- nodeLookup node "type" >>= readMessageType
  pure  Message { messageName        = mName
                , messageProbability = mProb
                , messageSource      = mSrc
                , messageTarget      = mTrgt
                , messageType        = mType
                }
  where readMessageType :: BS.ByteString -> Either Error MessageType
        readMessageType "synchronous"  = pure Synchronous
        readMessageType "asynchronous" = pure Asynchronous
        readMessageType s              = Left $ "Invalid message type: " <> s

-- | Read a Lifeline from a respective node
-- Example XML node containing a Lifeline
-- ```xml
-- <Lifeline name="Mock lifeline" reliability="0.999" />
-- ```
lifelineFromNode :: Node -> Either Error Lifeline
lifelineFromNode node = do
  lName <- nodeLookup node "name"
  lRel  <- nodeLookup node "reliability" >>= read'
  pure $ Lifeline { lifelineName        = lName
                  , lifelineReliability = lRel
                  }


-- | Read a Fragment from a respective node
-- Example XML node containing a Fragment
-- ```xml
-- <Fragment name="n6" type="optional">
--   <RepresentedBy seqDiagName="n6" />
-- </Fragment>
-- ```
fragmentFromNode :: Node -> Either Error Fragment
fragmentFromNode node = do
  fName <- nodeLookup node "name"
  fType <- nodeLookup node "type" >>= readType
  case children node of
    [x] | name x == "RepresentedBy" -> do
      repBy <- nodeLookup x "seqDiagName"
      pure $ Fragment { fragmentName = fName
                      , fragmentType = fType
                      , fragmentRepBy = Just repBy
                      }
    _   -> Left "Invalid Fragment node"

  where readType :: BS.ByteString -> Either Error FragmentType
        readType "optional" = pure Optional
        readType s           = Left $ "Invalid Fragment type " <> s

read' :: Read a => BS.ByteString -> Either Error a
read' s =
  case readMaybe $ C.unpack s of
    Nothing -> Left $ "Cannot parse value: " <> s
    Just x  -> pure x
