----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Types used everywhere.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Cell
        , Direction(..)
        , Location
        , Msg(..)
        , Operation(..)
        , Player
        , Row
        , WallSpec
        , Walls
        , operationToDirection
        , sumLocations
        )

import Array exposing (Array)
import Svg.Button as Button
import Window exposing (Size)


type Operation
    = TurnRight
    | TurnLeft
    | GoForward
    | GoBack


type Msg
    = InitialSize Size
    | Resize Size
    | DownKey Int
    | ButtonMsg (Button.Msg Msg Operation)
    | Nop


type Direction
    = North
    | South
    | East
    | West


operationToDirection : Operation -> Direction
operationToDirection operation =
    case operation of
        GoForward ->
            North

        GoBack ->
            South

        TurnLeft ->
            West

        TurnRight ->
            East


type alias Location =
    ( Int, Int )


sumLocations : Location -> Location -> Location
sumLocations ( r1, c1 ) ( r2, c2 ) =
    ( r1 + r2, c1 + c2 )


type alias Walls =
    { north : Bool
    , south : Bool
    , east : Bool
    , west : Bool
    }


type alias Player =
    { id : String
    , boardid : String
    , name : String
    , location : Location
    , direction : Direction
    }


type alias Cell =
    { location : Location
    , walls : Walls
    , players : List Player
    }


type alias Row =
    Array Cell


type alias Board =
    { id : String
    , rows : Int
    , cols : Int
    , contents : Array Row
    }


type alias BoardSpec =
    List String


type alias WallSpec =
    List Bool
