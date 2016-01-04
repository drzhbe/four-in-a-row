import Types exposing (Point, Color, Column, Board)
import BoardLogic exposing (createBoard, put, isFourInARow)
import Circle

--import Graphics.Element exposing (show)
import Array

import Signal exposing (Signal, Address)

import Html exposing (..)
import Html.Attributes exposing (style)
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
        --Remove x -> model - x


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

getColumnView : Int -> Address Action -> Color -> Board -> Html
getColumnView index address color board =
    div []
        [ button [ onClick address (Add index color) ] [ text ("add to " ++ (toString index) ++ " column") ]
        , div [] [ text (toString (Array.get index board)) ]
        ]

getVisibilityStyle : Bool -> Attribute
getVisibilityStyle visible =
    if visible
    then style [ ("display", "block") ]
    else style [ ("display", "none") ]



