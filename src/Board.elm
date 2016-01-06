module Board (Action, view) where

import Types exposing (Board, Cell, Color)
import Circle


import Array
import Svg exposing (Svg, svg, rect, g, text)
import Svg.Attributes exposing (..)
--import Svg.Events exposing (onClick)


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

size : Int
size = 100

type Action
    = Hack Circle.Action

viewCell : Signal.Address Action -> Cell -> Svg
viewCell address cell =
    Circle.embedView
        (Signal.forwardTo address Hack)
        (fst (Circle.init cell.x cell.y cell.color))

viewColumn : Signal.Address Action -> Array.Array Cell -> Svg
viewColumn address column =
    g
        []
        (Array.toList (Array.map (viewCell address) column))

view : Signal.Address Action -> Model -> Svg
view address model =
    svg
        [ width (toString (size * 7))
        , height (toString (size * 6))
        , viewBox ("0 0 " ++ (toString (size * 7)) ++ " " ++ (toString (size * 6)))
        ]
        (Array.toList (Array.map (viewColumn address) model.board))
     