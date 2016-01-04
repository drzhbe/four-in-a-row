module Circle (Model, Action, init, update, view) where

import Signal exposing (Signal, Address)
import Time exposing (Time, second)
import Html exposing (Html)
--import Html.Attributes exposing (style)
--import Html.Events exposing (onClick)
import Svg exposing (svg, rect, g, text)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects)


-- MOODEL

type alias Model =
    { x : Float
    , y : Float
    , animationState : AnimationState
    , finalY : Float
    }

type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }

size : Int
size = 100

init : Float -> Float -> (Model, Effects Action)
init x finalY =
    (
        { x = x
        , y = 0
        , animationState = Nothing
        , finalY = finalY
        }
    , Effects.none
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

view : Address Action -> Model -> Html
view address model =
    let
        currentY =
            model.y + toOffset model.animationState model.finalY
    in
        svg
            [ width (toString size)
            , height (toString (size * 2))
            , viewBox ("0 0 " ++ (toString size) ++ " " ++ (toString (size * 2)))
            ]
            [ g [ transform ("translate(0," ++ toString currentY ++ ")")
                , onClick (Signal.message address Fall)
                ]
                [ rect
                    [ x (toString model.x)
                    , y "0"
                    , width (toString size)
                    , height (toString size)
                    , rx "15"
                    , ry "15"
                    , style "fill: #60B5CC"
                    ]
                    []
                ]
            ]
