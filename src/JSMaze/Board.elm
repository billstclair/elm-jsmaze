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
        ( boardToStrings
        , makeEmptyBoard
        , separateBoardSpec
        , simpleBoard
        , simpleBoardSpec
        , stringsToBoard
        , stringsToBoardResult
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


simpleBoard : Board
simpleBoard =
    stringsToBoard simpleBoardSpec


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
    { nsSpecs : List WallSpec
    , ewSpecs : List WallSpec
    , rows : Int
    , cols : Int
    }


separateBoardSpec : BoardSpec -> Result String WallSpecs
separateBoardSpec spec =
    let
        split : BoardSpec -> ( CharListList, CharListList ) -> ( CharListList, CharListList )
        split =
            \lines ( nss, ews ) ->
                case lines of
                    [] ->
                        ( List.reverse nss, List.reverse ews )

                    [ _ ] ->
                        ( List.reverse nss, List.reverse ews )

                    ns :: ew :: tail ->
                        split tail ( oddChars ns :: nss, evenChars ew :: ews )

        ( nss, ews ) =
            split spec ( [], [] )

        nsls =
            List.map List.length nss

        ewls =
            List.map List.length ews

        maxnsl =
            List.foldl max 0 nsls

        minnsl =
            List.foldl min 1000 nsls

        maxewl =
            List.foldl max 0 ewls

        minewl =
            List.foldl min 1000 ewls
    in
    if minnsl == maxnsl && minewl == maxewl && minnsl == minewl then
        Ok
            { nsSpecs = List.map (charsEqual '-') nss
            , ewSpecs = List.map (charsEqual '|') ews
            , rows = List.length nss
            , cols = maxewl
            }
    else
        Err ("Length mismatch: " ++ toString nsls ++ ", " ++ toString ewls)


makeEmptyCell : Int -> Int -> Int -> Int -> Cell
makeEmptyCell rows cols rownum colnum =
    { location = ( rownum, colnum )
    , walls =
        { north = rownum == 0
        , south = rownum == (rows - 1)
        , west = colnum == 0
        , east = colnum == (cols - 1)
        }
    , players = []
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


type alias WallSetter =
    Int -> Bool -> Row -> Maybe Row -> ( Row, Maybe Row )


setWalls : WallSetter -> List WallSpec -> Board -> Board
setWalls setter wallSpecs board =
    let
        doCols : Int -> WallSpec -> Row -> Maybe Row -> ( Row, Maybe Row )
        doCols =
            \colnum specs row rowAbove ->
                case specs of
                    [] ->
                        ( row, rowAbove )

                    spec :: tail ->
                        let
                            ( nr, nra ) =
                                setter colnum spec row rowAbove
                        in
                        doCols (colnum + 1) tail nr nra

        doRows : Int -> List WallSpec -> Array Row -> Array Row
        doRows =
            \rownum wallspecs rows ->
                case wallspecs of
                    [] ->
                        rows

                    specs :: tail ->
                        case Array.get rownum rows of
                            Nothing ->
                                rows

                            Just row ->
                                let
                                    rowAbove =
                                        Array.get (rownum - 1) rows

                                    ( nr, nra ) =
                                        doCols 0 specs row rowAbove

                                    nrs =
                                        Array.set rownum nr rows

                                    newRows =
                                        case nra of
                                            Nothing ->
                                                nrs

                                            Just r ->
                                                Array.set (rownum - 1) r nrs
                                in
                                doRows (rownum + 1) tail newRows

        rows =
            doRows 0 wallSpecs board.contents
    in
    { board | contents = rows }


setNss =
    setWalls setNs


setNs : Int -> Bool -> Row -> Maybe Row -> ( Row, Maybe Row )
setNs colnum isNs row rowAbove =
    if not isNs then
        ( row, rowAbove )
    else
        ( setDir setN colnum row
        , case rowAbove of
            Nothing ->
                Nothing

            Just rw ->
                Just <| setDir setS colnum rw
        )


setEws =
    setWalls setEw


setEw : Int -> Bool -> Row -> Maybe Row -> ( Row, Maybe Row )
setEw colnum isEw row rowAbove =
    if not isEw then
        ( row, Nothing )
    else
        ( setDir setW colnum row
            |> setDir setE (colnum - 1)
        , Nothing
        )


type alias DirSetter =
    Walls -> Walls


setDir : DirSetter -> Int -> Row -> Row
setDir setter colnum row =
    case Array.get colnum row of
        Nothing ->
            row

        Just cell ->
            let
                newcell =
                    { cell | walls = setter cell.walls }
            in
            Array.set colnum newcell row


setE walls =
    { walls | east = True }


setW walls =
    { walls | west = True }


setN walls =
    { walls | north = True }


setS walls =
    { walls | south = True }


stringsToBoard : BoardSpec -> Board
stringsToBoard spec =
    case stringsToBoardResult spec of
        Ok board ->
            board

        Err _ ->
            makeEmptyBoard 1 1


stringsToBoardResult : BoardSpec -> Result String Board
stringsToBoardResult spec =
    case separateBoardSpec spec of
        Err msg ->
            Err msg

        Ok wallSpecs ->
            let
                rows =
                    wallSpecs.rows

                cols =
                    wallSpecs.cols
            in
            makeEmptyBoard rows cols
                |> setNss wallSpecs.nsSpecs
                |> setEws wallSpecs.ewSpecs
                |> Ok


boardToStrings : Board -> BoardSpec
boardToStrings board =
    let
        prefix =
            Array.toList board.contents
                |> List.concatMap rowToStrings

        last =
            String.repeat board.cols " -" ++ " "
    in
    List.append prefix [ last ]


rowToStrings : Row -> List String
rowToStrings row =
    [ rowToNorths row ++ " "
    , rowToWests row ++ "|"
    ]


rowToNorths : Row -> String
rowToNorths row =
    let
        north : Cell -> String
        north =
            \cell ->
                if cell.walls.north then
                    " -"
                else
                    "  "
    in
    Array.toList row
        |> List.map north
        |> String.concat


rowToWests : Row -> String
rowToWests row =
    let
        west : Cell -> String
        west =
            \cell ->
                if cell.walls.west then
                    "| "
                else
                    "  "
    in
    Array.toList row
        |> List.map west
        |> String.concat
