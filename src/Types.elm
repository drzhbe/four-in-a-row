module Types (Point, Color, Cell, Column, Board) where

import Array

type alias Point = (Int, Int)
type alias Color = Int
type alias Cell =
    { x : Int
    , y : Int
    , color : Color
    }
type alias Column = Array.Array Cell
type alias Board = Array.Array Column
