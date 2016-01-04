module Types (Point, Color, Column, Board) where

import Array

type alias Point = (Int, Int)
type alias Color = Int
type alias Column = Array.Array Color
type alias Board = Array.Array Column
