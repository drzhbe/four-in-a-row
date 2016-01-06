import Types exposing (Point, Color, Cell, Column, Board)
import BoardLogic exposing (createBoard, put, isFourInARow)
import Board

--import Graphics.Element exposing (show)
import Array

import Signal exposing (Signal, Address)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import StartApp
import Task
import Effects exposing (Effects, Never)


app : StartApp.App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }
    --StartApp.start
    --    { init = Circle.init 0 100
    --    , update = Circle.update
    --    , view = Circle.view
    --    , inputs = []
    --    }

port tasks : Signal (Task.Task Never())
port tasks =
    app.tasks

main : Signal Html
main =
    app.html


type alias Model =
    { board : Board
    , turn : Color
    , gameFinished : Bool
    --debug parameters
    , horizontal : Int
    , vertical : Int
    , backSlash : Int
    , slash : Int
    }

init : (Model, Effects Action)
init =
    (
        { board = createBoard
        , turn = 1
        , gameFinished = False
        , horizontal = 0
        , vertical = 0
        , backSlash = 0
        , slash = 0
        }
    , Effects.none
    )

changeTurn : Color -> Color
changeTurn color =
    if color == 1 then 2
    else 1



type Action
    = NoOp
    | Restart
    | Add Int Color
    | ToBoard Board.Action
    --| Remove Int

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        NoOp ->
            ( model
            , Effects.none
            )
        Restart -> init
        Add x color ->
            let
                (point, newBoard) = put x color model.board
            in
                if newBoard == model.board
                then
                    ( model
                    , Effects.none
                    )
                else
                    let
                        (finished, horizontal, vertical, backSlash, slash) =
                            isFourInARow point color model.board
                    in
                        if finished
                        then
                            (
                                { model
                                | board = newBoard
                                , horizontal = horizontal
                                , vertical = vertical
                                , backSlash = backSlash
                                , slash = slash
                                , gameFinished = True
                                }
                            , Effects.none
                            )
                        else
                            (
                                { model
                                | board = newBoard
                                , horizontal = horizontal
                                , vertical = vertical
                                , backSlash = backSlash
                                , slash = slash
                                , turn = changeTurn model.turn
                                }
                            , Effects.none
                            )
        ToBoard action ->
            ( model
            , Effects.none
            )
        --Modify id circleAction ->
        --    let
        --        updateCircle (circleID, circleModel) =
        --            if circleID == id
        --            then (circleID, Circle.update circleAction circleModel)
        --            else (circleID, circleModel)
        --    in
        --        { model  }


        --Remove x -> model - x


-- VIEW

type alias ID = Point

--viewCircle : Address Action -> Circle.Model -> Html
--viewCircle address model =
--    Circle.view (Signal.forwardTo address Hack) model

--getCellView : Address Action -> Int -> Int -> Cell -> Html
--getCellView address x y cell =
--    if cell.color == 0
--    then div [] []
--    else viewCircle address (fst (Circle.init x y cell.color))

getColumnView : Int -> Address Action -> Color -> Board -> Html
getColumnView x address color board =
    --let
    --    getCellViewClosure y cell =
    --        getCellView address x y cell
    --in
        case Array.get x board of
            Nothing -> div [] []
            Just column ->
                div [ class "column"
                    --, style
                    --    [ ("float", "left")
                    --    , ("height", "700px")
                    --    ]
                    ]
                    [ button [ onClick address (Add x color) ] [ text ("add to " ++ (toString x) ++ " column") ] ]
                    --:: (Array.toList (Array.indexedMap getCellViewClosure column))


    --div []
    --    [ button [ onClick address (Add index color) ] [ text ("add to " ++ (toString index) ++ " column") ]
    --    , div [] [ text (toString (Array.get index board)) ]
    --    ]


getVisibilityStyle : Bool -> Attribute
getVisibilityStyle visible =
    if visible
    then style [ ("display", "block") ]
    else style [ ("display", "none") ]


view : Address Action -> Model -> Html
view address model =
    div []
        [ div []
            [ getColumnView 0 address model.turn model.board
            , getColumnView 1 address model.turn model.board
            , getColumnView 2 address model.turn model.board
            , getColumnView 3 address model.turn model.board
            , getColumnView 4 address model.turn model.board
            , getColumnView 5 address model.turn model.board
            , getColumnView 6 address model.turn model.board
            , Board.view (Signal.forwardTo address ToBoard) model
            ]
        , div []
            [ text (
                "Last check horizontal: " ++ (toString model.horizontal)
                ++ "\n vertical: " ++ (toString  model.vertical)
                ++ "\n backSlash: " ++ (toString  model.backSlash)
                ++ "\n slash: " ++ (toString  model.slash)
            ) ]
        , div [ getVisibilityStyle model.gameFinished ]
            [ text ("The winner is " ++ (toString model.turn)) ]
        , button [ onClick address Restart ] [ text "restart" ]
        ]



