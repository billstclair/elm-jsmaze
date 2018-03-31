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
        ( addPlayer
        , boardToStrings
        , canMove
        , fixPlayer
        , forwardDelta
        , getCell
        , makeEmptyBoard
        , removePlayer
        , resize
        , separateBoardSpec
        , setCell
        , setId
        , simpleBoard
        , simpleBoardSpec
        , stringsToBoard
        , stringsToBoardResult
        , updatePlayer
        )

import Array exposing (Array)
import Array.Extra as AE
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
        , sumLocations
        )


oldSimpleBoardSpec : BoardSpec
oldSimpleBoardSpec =
    [ "--------"
    , "||      "
    , "  ----- "
    , "| |     "
    , " - -----"
    , "||      "
    , " ------ "
    , "|       "
    , "--------"
    ]


simpleBoardSpec : BoardSpec
simpleBoardSpec =
    [ "----------"
    , "||        "
    , "  ------- "
    , "| |      |"
    , "   -----  "
    , "|| |    ||"
    , "    ---   "
    , "||| |  |||"
    , "     -    "
    , "||||| | ||"
    , "    -     "
    , "||||   | |"
    , "   ----   "
    , "|||     | "
    , "  ------  "
    , "||       |"
    , " -------- "
    , "|         "
    , "----------"
    ]


simpleBoard : Board
simpleBoard =
    stringsToBoard "simple" simpleBoardSpec


type alias CharList =
    List Char


type alias CharListList =
    List CharList


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
                        split tail ( String.toList ns :: nss, String.toList ew :: ews )

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
    { id = "empty"
    , rows = rows
    , cols = cols
    , contents = Array.fromList contents
    }


setId : String -> Board -> Board
setId id board =
    { board | id = id }


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


stringsToBoard : String -> BoardSpec -> Board
stringsToBoard id spec =
    case stringsToBoardResult spec of
        Ok board ->
            { board | id = id }

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
            String.repeat board.cols "-"
    in
    List.append prefix [ last ]


rowToStrings : Row -> List String
rowToStrings row =
    [ rowToNorths row
    , rowToWests row
    ]


rowToNorths : Row -> String
rowToNorths row =
    let
        north : Cell -> String
        north =
            \cell ->
                if cell.walls.north then
                    "-"
                else
                    " "
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
                    "|"
                else
                    " "
    in
    Array.toList row
        |> List.map west
        |> String.concat


modifyPlayers : (Player -> List Player -> List Player) -> Player -> Board -> Board
modifyPlayers modifier player board =
    let
        ( rownum, colnum ) =
            player.location

        id =
            player.id
    in
    case Array.get rownum board.contents of
        Nothing ->
            board

        Just row ->
            case Array.get colnum row of
                Nothing ->
                    board

                Just cell ->
                    let
                        newCell =
                            { cell
                                | players =
                                    modifier player cell.players
                            }

                        newRow =
                            Array.set colnum newCell row

                        newContents =
                            Array.set rownum newRow board.contents
                    in
                    { board | contents = newContents }


removeFromPlayers : Player -> List Player -> List Player
removeFromPlayers player players =
    List.filter ((\id p -> id == p.id) player.id) players


removePlayer : Player -> Board -> Board
removePlayer player board =
    modifyPlayers removeFromPlayers player board


addPlayer : Player -> Board -> Board
addPlayer player board =
    modifyPlayers (::) player board


updatePlayer : Player -> Player -> Board -> Board
updatePlayer player newPlayer board =
    removePlayer player board
        |> addPlayer newPlayer


getCell : Location -> Board -> Maybe Cell
getCell ( r, c ) board =
    case Array.get r board.contents of
        Nothing ->
            Nothing

        Just row ->
            Array.get c row


setCell : Location -> Cell -> Board -> Board
setCell ( r, c ) cell board =
    case Array.get r board.contents of
        Nothing ->
            board

        Just row ->
            let
                newRow =
                    Array.set c cell row

                newContents =
                    Array.set r newRow board.contents
            in
            { board | contents = newContents }


maxSize : Int
maxSize =
    20


minSize : Int
minSize =
    3


sizeLimit : Int -> Int
sizeLimit size =
    max minSize <| min maxSize size


resize : ( Int, Int ) -> Board -> Board
resize ( newrows, newcols ) board =
    let
        ( r, c ) =
            ( sizeLimit newrows, sizeLimit newcols )

        ( or, oc ) =
            ( board.rows, board.cols )

        ( minr, minc ) =
            ( min r or, min c oc )
    in
    if r == or && c == oc then
        board
    else
        let
            players =
                collectPlayers board

            nb0 =
                makeEmptyBoard r c

            nb =
                { nb0 | id = board.id }

            colnums =
                List.range 0 (c - 1)

            colLoop : Int -> Int -> Row -> Row
            colLoop =
                \rowidx colidx row ->
                    if colidx >= minc then
                        row
                    else
                        let
                            cell =
                                case getCell ( rowidx, colidx ) board of
                                    Nothing ->
                                        makeEmptyCell r c rowidx colidx

                                    Just cl ->
                                        cl
                        in
                        colLoop
                            rowidx
                            (colidx + 1)
                        <|
                            Array.set colidx cell row

            rowLoop : Int -> List Row -> Array Row
            rowLoop =
                \rowidx rows ->
                    if rowidx >= minr then
                        let
                            emptyRows =
                                if r <= or then
                                    []
                                else
                                    List.map
                                        (\ri ->
                                            makeEmptyRow r c colnums ri
                                        )
                                        (List.range rowidx (r - 1))
                        in
                        Array.fromList <|
                            List.concat [ List.reverse rows, emptyRows ]
                    else
                        let
                            row =
                                case Array.get rowidx board.contents of
                                    Nothing ->
                                        makeEmptyRow r c colnums rowidx

                                    Just rw ->
                                        if c == oc then
                                            rw
                                        else if c < oc then
                                            AE.sliceUntil c rw
                                        else
                                            let
                                                newcells =
                                                    makeEmptyRow r c colnums rowidx
                                                        |> Array.toList

                                                oldcells =
                                                    Array.toList rw
                                            in
                                            List.concat
                                                [ oldcells
                                                , List.drop oc newcells
                                                ]
                                                |> Array.fromList

                            row2 =
                                colLoop rowidx 0 row
                        in
                        rowLoop (rowidx + 1) <| row2 :: rows
        in
        { nb | contents = rowLoop 0 [] }
            |> addPlayers players


collectPlayers : Board -> List Player
collectPlayers board =
    List.concatMap
        (\row ->
            List.concatMap .players <| Array.toList row
        )
    <|
        Array.toList board.contents


fixPlayer : Board -> Player -> Player
fixPlayer board player =
    let
        maxrow =
            board.rows - 1

        maxcol =
            board.cols - 1

        ( r, c ) =
            player.location
    in
    { player | location = ( min maxrow r, min maxcol c ) }


addPlayers : List Player -> Board -> Board
addPlayers players board =
    List.foldl addPlayer board <| List.map (fixPlayer board) players


canMove : Location -> Location -> Board -> Bool
canMove location ( dr, dc ) board =
    let
        ( r, c ) =
            location

        ( nr, nc ) =
            ( r + dr, c + dc )
    in
    if nr < 0 || nr >= board.rows || nc < 0 || nc >= board.cols then
        False
    else
        case getCell location board of
            Nothing ->
                False

            Just cell ->
                let
                    walls =
                        cell.walls

                    drok =
                        if dr == 0 then
                            True
                        else if dr > 0 then
                            not walls.south
                        else
                            not walls.north

                    dcok =
                        if dc == 0 then
                            True
                        else if dc > 0 then
                            not walls.east
                        else
                            not walls.west
                in
                drok && dcok


type alias WallGetter =
    Walls -> Bool


forwardDelta : Direction -> ( Location, Direction, Direction, WallGetter, WallGetter, WallGetter )
forwardDelta dir =
    case dir of
        North ->
            ( ( -1, 0 ), West, East, .west, .east, .north )

        South ->
            ( ( 1, 0 ), East, West, .east, .west, .south )

        East ->
            ( ( 0, 1 ), North, South, .north, .south, .east )

        West ->
            ( ( 0, -1 ), South, North, .south, .north, .west )
