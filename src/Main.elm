module Main exposing (Model, Msg(..), NodeData, configuration, expandAllCollapseAllButtons, initialModel, main, nodeLabel, nodeUid, selectedNodeDetails, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Tree as T
import TreeView as TV


type alias NodeData =
    { uid : String
    , label : String
    }


type alias Model =
    { rootNodes : List (T.Node NodeData)
    , treeModel : TV.Model NodeData String Never ()
    , selectedNode : Maybe NodeData
    }


nodeLabel : T.Node NodeData -> String
nodeLabel n =
    case n of
        T.Node node ->
            node.data.label


nodeUid : T.Node NodeData -> TV.NodeUid String
nodeUid n =
    case n of
        T.Node node ->
            TV.NodeUid node.data.uid


configuration : TV.Configuration NodeData String
configuration =
    TV.Configuration nodeUid nodeLabel TV.defaultCssClasses


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    let
        rootNodes =
            [ T.Node
                { children =
                    [ T.Node { children = [], data = NodeData "1.1" "hello" }
                    , T.Node { children = [], data = NodeData "1.2" "goodbye" }
                    , T.Node { children = [], data = NodeData "1.3" "you say yes" }
                    ]
                , data = NodeData "1" "Beatles"
                }
            , T.Node
                { children =
                    [ T.Node
                        { children =
                            [ T.Node { children = [], data = NodeData "2.1.1" "la" }
                            , T.Node { children = [], data = NodeData "2.1.2" "vista" }
                            ]
                        , data = NodeData "2.1" "hasta"
                        }
                    , T.Node
                        { children = []
                        , data = NodeData "2.2" "baby"
                        }
                    ]
                , data = NodeData "2" "Terminator"
                }
            ]
    in
    ( { rootNodes = rootNodes
      , treeModel = TV.initializeModel configuration rootNodes
      , selectedNode = Nothing
      }
    , Cmd.none
    )


type Msg
    = TreeViewMsg (TV.Msg String)
    | ExpandAll
    | CollapseAll


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        treeModel =
            case message of
                TreeViewMsg tvMsg ->
                    TV.update tvMsg model.treeModel

                ExpandAll ->
                    TV.expandAll model.treeModel

                CollapseAll ->
                    TV.collapseAll model.treeModel
    in
    ( { model
        | treeModel = treeModel
        , selectedNode = TV.getSelected treeModel |> Maybe.map .node |> Maybe.map T.dataOf
      }
    , Cmd.none
    )


expandAllCollapseAllButtons : Html Msg
expandAllCollapseAllButtons =
    div
        []
        [ button [ onClick ExpandAll ] [ text "expand" ]
        , button [ onClick CollapseAll ] [ text "collapse" ]
        ]


selectedNodeDetails : Model -> Html Msg
selectedNodeDetails model =
    let
        selectedDetails =
            Maybe.map (\nodeData -> nodeData.uid ++ ": " ++ nodeData.label) model.selectedNode
                |> Maybe.withDefault "(nothing selected)"
    in
    div
        []
        [ text selectedDetails
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ expandAllCollapseAllButtons
        , selectedNodeDetails model
        , map TreeViewMsg <| TV.view model.treeModel
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TreeViewMsg (TV.subscriptions model.treeModel)


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
