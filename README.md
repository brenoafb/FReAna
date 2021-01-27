# FReAna

- Funcional toolkit for reliability analysis of product lines.

This project aims to implement some useful tools and techniques
for the analysis of Software Product Lines (SPLs) in Haskell.

It builds upon the work done in the [ReAna](https://github.com/SPLMC/reana-spl) project.

## Usage

This project uses the [Stack](https://docs.haskellstack.org/en/stable/README/) tool.

To build the project.

```sh
$ stack build
```

The current tool reads an XML model file and pretty print the model read.

```sh
$ stack run <XML model file>
```

## Model Files

A model file specifies a behavioral model of a piece of software.
It is composed of an activity diagram, as well as multiple sequence diagrams,
one for each "feature"

Here is an example

```xml
<?xml version="1.0" encoding="UTF-8"?>
<SplBehavioralModel name="translatedBSN">

  <ActivityDiagram name="AD_SPL_0">
    <Elements>
      <ActivityDiagramElement name="Start node" type="StartNode" />
      <ActivityDiagramElement name="Capture" type="Activity">
        <RepresentedBy seqDiagName="Capture" />
      </ActivityDiagramElement>
    </Elements>
    <Transitions>
      <Transition name="Capture" probability="1.0" source="Start node" target="Capture" />
    </Transitions>
  </ActivityDiagram>

  <SequenceDiagrams>
    <SequenceDiagram guard="true" name="QoSChange">
      <Message name="" probability="0.999" source="Mock lifeline" target="Lifeline_0" type="asynchronous" />
    </SequenceDiagram>
    <Lifelines>
      <Lifeline name="Mock lifeline" reliability="0.999" />
    </Lifelines>
    <Fragments>
      <Fragment name="n6" type="optional">
        <RepresentedBy seqDiagName="n6" />
      </Fragment>
    </Fragments>
  </SequenceDiagrams>

</SplBehavioralModel>
```

For this file, the program produces the following output.

```
Model
    { mName = "translatedBSN"
    , mActivityDiagram = ActivityDiagram
        { adElements =
            [ ActivityDiagramElement
                { adElementName = "Start node"
                , adElementType = "StartNode"
                , adElementRepBy = Nothing
                }
            , ActivityDiagramElement
                { adElementName = "Capture"
                , adElementType = "Activity"
                , adElementRepBy = Just "Capture"
                }
            ]
        , adTransitions =
            [ Transition
                { transitionName = "Capture"
                , transitionProbability = 1.0
                , transitionSource = "Start node"
                , transitionTarget = "Capture"
                }
            ]
        }
    , mSequenceDiagrams = SequenceDiagrams
        { sdsSequenceDiagrams =
            [ SequenceDiagram
                { sdName = "QoSChange"
                , sdGuard = "true"
                , sdMessages =
                    [ Message
                        { messageName = ""
                        , messageProbability = 0.999
                        , messageSource = "Mock lifeline"
                        , messageTarget = "Lifeline_0"
                        , messageType = Asynchronous
                        }
                    ]
                , sdFragments = []
                }
            ]
        , sdsLifelines =
            [ Lifeline
                { lifelineName = "Mock lifeline"
                , lifelineReliability = 0.999
                }
            ]
        , sdsFragments =
            [ Fragment
                { fragmentName = "n6"
                , fragmentType = Optional
                , fragmentRepBy = Just "n6"
                }
            ]
        }
    }
```
