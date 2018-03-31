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
        , Layout(..)
        , Location
        , Model
        , Msg(..)
        , Operation(..)
        , Player
        , Row
        , SavedModel
        , WallSpec
        , Walls
        , currentBoardId
        , currentPlayerId
        , defaultSavedModel
        , initialPlayer
        , operationToDirection
        , sumLocations
        )

import Array exposing (Array)
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes as LST exposing (Key, Ports, Value)
import Svg.Button as Button exposing (Button)
import Time exposing (Time)
import Window exposing (Size)


type alias Model =
    { windowSize : Size
    , board : Board
    , player : Player
    , layout : Layout
    , isTouchAware : Bool
    , forwardButton : Button Operation
    , backButton : Button Operation
    , subscription : Maybe ( Time, Button.Msg Msg Operation )
    , storage : LocalStorage Msg
    }


type alias SavedModel =
    { layout : Layout
    }


defaultSavedModel : SavedModel
defaultSavedModel =
    { layout = NormalLayout
    }


type Layout
    = NormalLayout
    | TopViewLayout
    | EditingLayout
    | NoLayout


type Operation
    = TurnRight
    | TurnLeft
    | GoForward
    | GoBack
    | ToggleLayout
    | ToggleWall Direction Location
    | AddColumn Int
    | AddRow Int
    | EditMaze
    | GetMaze
    | SaveMaze


type Msg
    = InitialSize Size
    | Resize Size
    | DownKey Int
    | ButtonMsg (Button.Msg Msg Operation)
    | UpdatePorts LST.Operation (Maybe (Ports Msg)) Key Value
    | DoWrite (List (LocalStorage Msg -> Cmd Msg))
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

        _ ->
            North


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


currentPlayerId : String
currentPlayerId =
    "currentPlayer"


type alias Player =
    { id : String
    , boardid : String
    , name : String
    , location : Location
    , direction : Direction
    }


initialPlayer : Player
initialPlayer =
    { id = currentPlayerId
    , boardid = currentBoardId
    , name = "Joe Bob"
    , location = ( 0, 0 )
    , direction = South
    }


type alias Cell =
    { location : Location
    , walls : Walls
    , players : List Player
    }


type alias Row =
    Array Cell


currentBoardId : String
currentBoardId =
    "current"


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
