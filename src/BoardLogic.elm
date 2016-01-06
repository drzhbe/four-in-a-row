module BoardLogic (createBoard, put, isFourInARow) where

import Types exposing (Point, Color, Cell, Column, Board)
import Array

boardWidth : Int
boardWidth = 7
boardHeight : Int
boardHeight = 6


initCell : Color -> Int -> Int -> Cell
initCell color x y =
    { x = x * 100
    , y = (boardHeight - y - 1) * 100 -- -1 because we start to draw from top corner
    , color = color
    }

createBoard : Board
createBoard =
    Array.initialize 7 (\x -> Array.initialize 6 (initCell 0 x))

find : Color -> Column -> Int
find value array =
    findIterate 0 value array

findIterate : Int -> Color -> Column -> Int
findIterate index value array =
    case Array.get index array of
        Nothing -> -1
        Just cell ->
            if cell.color == value then index
            else findIterate (index + 1) value array

put : Int -> Color -> Board -> (Point, Board)
put x color board =
    case Array.get x board of
        Nothing -> ( (-1, -1), board )
        Just column ->
            let
                (y, newColumn) = putIntoColumn color x column
            in
                if newColumn == column
                then ( (-1, -1), board )
                else ( (x, y), Array.set x newColumn board )


putIntoColumn : Color -> Int -> Column -> (Int, Column)
putIntoColumn color x column =
    let
        index = find 0 column
    in
        if index == -1 then ( -1, column )
        else ( index, Array.set index (initCell color x index) column )

isFourInARow : Point -> Color -> Board -> (Bool, Int, Int, Int, Int)
isFourInARow point color board =
    let
        (h, v, bs, s) =
            ( checkHorizontal point color board
            , checkVertical point color board
            , checkBackSlash point color board
            , checkSlash point color board
            )
    in
        ( h >= 4 || v >= 4 || bs >= 4 || s >= 4
        , h
        , v
        , bs
        , s
        )

checkHorizontal : Point -> Color -> Board -> Int
checkHorizontal point color board =
    check
        right
        (right point)
        color
        board
        (check
            left
            (left point)
            color
            board
            1
        )

checkVertical : Point -> Color -> Board -> Int
checkVertical point color board =
    check
        bottom
        (bottom point)
        color
        board
        1

checkBackSlash : Point -> Color -> Board -> Int
checkBackSlash point color board =
    check
        topLeft
        (topLeft point)
        color
        board
        (check
            bottomRight
            (bottomRight point)
            color
            board
            1
        )

checkSlash : Point -> Color -> Board -> Int
checkSlash point color board =
    check
        topRight
        (topRight point)
        color
        board
        (check
            bottomLeft
            (bottomLeft point)
            color
            board
            1
        )

check : (Point -> Point) -> Point -> Color -> Board -> Int -> Int
check getNextPoint point color board sum =
    case Array.get (fst point) board of
        Nothing -> sum
        Just column ->
            case Array.get (snd point) column of
                Nothing -> sum
                Just neighborCell ->
                    if neighborCell.color == color
                    then sum + (check getNextPoint (getNextPoint point) color board sum)
                    else sum

left : Point -> Point
left point =
    ((fst point) - 1, snd point)

right : Point -> Point
right point =
    ((fst point) + 1, snd point)

top : Point -> Point
top point =
    (fst point, (snd point) + 1)

bottom : Point -> Point
bottom point =
    (fst point, (snd point) - 1)

topLeft : Point -> Point
topLeft point =
    ((fst point) - 1, (snd point) + 1)

topRight : Point -> Point
topRight point =
    ((fst point) + 1, (snd point) + 1)

bottomLeft : Point -> Point
bottomLeft point =
    ((fst point) - 1, (snd point) - 1)

bottomRight : Point -> Point
bottomRight point =
    ((fst point) + 1, (snd point) - 1)
