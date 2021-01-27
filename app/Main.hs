{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Data
import Xeno.DOM
import Xeno.SAX
import Xeno.Types
import Text.Pretty.Simple (pPrint)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Generics.Uniplate.Data as U

filename = "bsn-reduced.xml"

main :: IO ()
main = do
  xmlText <- BS.readFile filename
  let processParams =
        Process { openF    = \t   -> BS.putStrLn $ "openF: "    <> t
                , attrF    = \t r -> BS.putStrLn $ "attrF: "    <> t <> ", " <> r
                , endOpenF = \t   -> BS.putStrLn $ "endOpenF: " <> t
                , textF    = \t   -> BS.putStrLn $ "textF: "    <> t
                , closeF   = \t   -> BS.putStrLn $ "closeF: "   <> t
                , cdataF   = \t   -> BS.putStrLn $ "cdataF: "   <> t
                }
  process processParams xmlText
  case parse xmlText of
    Left err -> print err
    Right node -> return ()

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
adTransitionFromNode = undefined

-- | Read a SequenceDiagrams object from a respective node
-- Example XML node containing a SequenceDiagrams object
-- ```xml
-- <Transition name="Capture" probability="1.0" source="Start node" target="Capture" />
-- ```
sdsFromNode :: Node -> Either Error SequenceDiagrams
sdsFromNode = undefined

-- | Read a SequenceDiagram from a respective node.
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
sdFromNode :: Node -> Either Error SequenceDiagram
sdFromNode = undefined

-- | Read a Lifeline from a respective node
-- Example XML node containing a Lifeline
-- ```xml
-- <Lifeline name="Mock lifeline" reliability="0.999" />
-- ```
lifelineFromNode :: Node -> Either Error Lifeline
lifelineFromNode = undefined

-- | Read a Fragment from a respective node
-- Example XML node containing a Fragment
-- ```xml
-- <Fragment name="n6" type="optional">
--   <RepresentedBy seqDiagName="n6" />
-- </Fragment>
-- ```
fragmentFromNode :: Node -> Either Error Fragment
fragmentFromNode = undefined

parseFile :: FilePath -> IO (Either XenoException Node)
parseFile f = do
  xml <- BS.readFile f
  pure $ parse xml

data Model =
  Model { mName             :: BS.ByteString
        , mActivityDiagram  :: ActivityDiagram
        , mSequenceDiagrams :: SequenceDiagrams
        } deriving (Eq, Show)


data ActivityDiagram =
  ActivityDiagram { adElements    :: [ActivityDiagramElement]
                  , adTransitions :: [Transition]
                  } deriving (Eq, Show)

data ActivityDiagramElement =
  ActivityDiagramElement { adElementName  :: BS.ByteString
                         , adElementType  :: BS.ByteString
                         , adElementRepBy :: Maybe BS.ByteString
                         } deriving (Eq, Show)

data Transition =
  Transition { transitionName        :: BS.ByteString
             , transitionProbability :: Double
             , transitionSource      :: BS.ByteString
             , transitionTarget      :: BS.ByteString
             } deriving (Eq, Show)

data SequenceDiagrams =
  SequenceDiagrams { sdSequenceDiagrams :: [SequenceDiagram]
                   , sdLifelines        :: [Lifeline]
                   , sdFragments        :: [Fragment]
                   } deriving (Eq, Show)

data SequenceDiagram =
  SequenceDiagram { sdName       :: BS.ByteString
                  , sdGuard      :: BS.ByteString  -- presence condition
                  , sdComponents :: [Either Message Fragment]
                  } deriving (Eq, Show)

data Message =
  Message { messageName        :: BS.ByteString
          , messageProbability :: Double
          , messageSource      :: BS.ByteString       -- lifeline
          , messageTarget      :: BS.ByteString       -- lifeline
          , messageType        :: MessageType
          } deriving (Eq, Show)

data MessageType = Synchronous | Asynchronous
  deriving (Eq, Show)

data Lifeline =
  Lifeline { lifelineName        :: BS.ByteString
           , lifelineReliability :: Double
           } deriving (Eq, Show)

data Fragment =
  Fragment { fragmentName :: BS.ByteString
           , fragmentType :: FragmentType
           , fragmentRepBy :: Maybe BS.ByteString  -- seqDiagName
           } deriving (Eq, Show)

data FragmentType = Optional
  deriving (Eq, Show)
