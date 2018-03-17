----------------------------------------------------------------------
--
-- Board.elm
-- Functions for maintaining the state of the JSMaze board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.Board
    exposing
        ( makeEmptyBoard
        , separateBoardSpec
        , simpleBoardSpec
        , stringsToBoard
        )

import Array exposing (Array)
import Debug exposing (log)
import JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Cell
        , Direction(..)
        , Location
        , Player
        , Row
        , WallSpec
        , Walls
        )


simpleBoardSpec : BoardSpec
simpleBoardSpec =
    [ " --------------- "
    , "| |             |"
    , "     ---------   "
    , "|   |           |"
    , "   -   --------- "
    , "| |             |"
    , "   -----------   "
    , "|               |"
    , " --------------- " -- Last row is extraneous
    ]


type alias CharList =
    List Char


type alias CharListList =
    List CharList


evenChars : String -> CharList
evenChars string =
    let
        chars =
            String.toList string

        evens : CharList -> CharList -> CharList
        evens =
            \chars res ->
                case chars of
                    [] ->
                        List.reverse res

                    [ _ ] ->
                        List.reverse res

                    ch :: _ :: tail ->
                        evens tail (ch :: res)
    in
    evens chars []


oddChars : String -> CharList
oddChars string =
    evenChars <| String.dropLeft 1 string


charsEqual : Char -> CharList -> WallSpec
charsEqual ch chs =
    List.map ((==) ch) chs


type alias WallSpecs =
    { hs : List WallSpec
    , vs : List WallSpec
    , rows : Int
    , cols : Int
    }


separateBoardSpec : BoardSpec -> Result String WallSpecs
separateBoardSpec spec =
    let
        split : BoardSpec -> ( CharListList, CharListList ) -> ( CharListList, CharListList )
        split =
            \lines ( hs, vs ) ->
                case lines of
                    [] ->
                        ( List.reverse hs, List.reverse vs )

                    [ _ ] ->
                        ( List.reverse hs, List.reverse vs )

                    h :: v :: tail ->
                        split tail ( oddChars h :: hs, evenChars v :: vs )

        ( hs, vs ) =
            split spec ( [], [] )

        hl =
            List.map List.length hs

        vl =
            List.map List.length vs

        maxhl =
            List.foldl max 0 hl

        minhl =
            List.foldl min 1000 hl

        maxvl =
            List.foldl max 0 vl

        minvl =
            List.foldl min 1000 vl
    in
    if minhl == maxhl && minvl == maxvl && minhl == minvl then
        Ok
            { hs = List.map (charsEqual '-') hs
            , vs = List.map (charsEqual '|') vs
            , rows = List.length hs
            , cols = maxhl
            }
    else
        Err ("Length mismatch: " ++ toString hl ++ ", " ++ toString vl)


makeEmptyCell : Int -> Int -> Int -> Int -> Cell
makeEmptyCell rows cols rownum colnum =
    { location = ( rownum, colnum )
    , walls =
        { north = rownum == 0
        , south = rownum == (rows - 1)
        , west = colnum == 0
        , east = colnum == (cols - 1)
        }
    , players = Array.empty
    }


makeEmptyRow : Int -> Int -> List Int -> Int -> Row
makeEmptyRow rows cols colnums rownum =
    List.map (makeEmptyCell rows cols rownum) colnums
        |> Array.fromList


makeEmptyBoard : Int -> Int -> Board
makeEmptyBoard rows cols =
    let
        rownums =
            List.range 0 (rows - 1)

        colnums =
            List.range 0 (cols - 1)

        contents =
            List.map (makeEmptyRow rows cols colnums) rownums
    in
    { rows = rows
    , cols = cols
    , contents = Array.fromList contents
    }


stringsToBoard : List String -> Maybe Board
stringsToBoard strings =
    Nothing
