module Circle (Model, Action, init, update, view, embedView) where

import Signal exposing (Signal, Address)
import Time exposing (Time, second, timestamp)
--import Html exposing (Html)
--import Html.Attributes exposing (style)
--import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect, g, text)
import Svg.Attributes exposing (..)
--import Svg.Events exposing (onClick)
import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects, Never)


-- MOODEL

type alias Model =
    { x : Float
    , y : Float
    , animationState : AnimationState
    , finalY : Float
    , color : String
    }

type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }

type alias Context =
    { actions : Address Action
    }

size : Int
size = 100

init : Int -> Int -> Int -> (Model, Effects Action)
init x finalY color =
    (
        { x = toFloat x
        , y = 0
        , animationState = Nothing
        , finalY = toFloat finalY
        --, finalY = toFloat (6 * size - finalY * size)
        , color = getColor color
        }
    , Effects.tick Tick
    )

duration : Time
duration = second

-- UPDATE

type Action =
    Fall
    | Tick Time

update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Fall ->
            case model.animationState of
                Nothing ->
                    ( model, Effects.tick Tick )
                Just _ ->
                    ( model, Effects.none )
        Tick clockTime ->
            let
                newElapsedTime =
                    case model.animationState of
                        Nothing -> 0
                        Just { elapsedTime, prevClockTime } ->
                            elapsedTime + (clockTime - prevClockTime)
            in
                if newElapsedTime > duration then
                    ( { model
                      | y = model.y + model.finalY
                      , animationState = Nothing
                      }
                    , Effects.none
                    )
                else
                    ( { model
                      | animationState =
                            Just
                                { elapsedTime = newElapsedTime
                                , prevClockTime = clockTime
                                }
                      }
                    , Effects.tick Tick
                    )


-- VIEW

toOffset : AnimationState -> Float -> Float
toOffset animationState finalY =
    case animationState of
        Nothing -> 0
        Just { elapsedTime } ->
            ease easeOutBounce float 0 finalY duration elapsedTime

getColor : Int -> String
getColor color =
    -- "#7FD13B" green
    if color == 0
    then "#888" -- gray
    else
        if color == 1
        then "#60B5CC" -- blue
        else "#F0AD00" -- yellow


view : Address Action -> Model -> Svg
view address model =
    svg
        [ width (toString size)
        , height (toString (size * 2))
        , viewBox ("0 0 " ++ (toString size) ++ " " ++ (toString (size * 2)))
        ]
        [ embedView address model ]


embedView : Address Action -> Model -> Svg
embedView address model =
    let
        currentY =
            model.y + toOffset model.animationState model.finalY
    in
        rect
            [ transform ("translate(0," ++ toString currentY ++ ")")
            , x (toString model.x)
            , y (toString model.finalY)
            --, y "0"
            , width (toString size)
            , height (toString size)
            , rx "15"
            , ry "15"
            , style ("fill: " ++ model.color)
            ]
            []
