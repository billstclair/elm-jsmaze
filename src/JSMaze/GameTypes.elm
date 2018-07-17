----------------------------------------------------------------------
--
-- GameTypes.elm
-- Types for multi-user games
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module GameTypes
    exposing
        ( Appearance(..)
        , Game
        , GamePlayer
        , Image(..)
        , PlayerName
        , Point
        , WallImage
        , WallImages
        )

import Array exposing (Array)
import Dict exposing (Dict)
import JSMaze.SharedTypes
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
import WebSocketFramework.Types exposing (GameId, PlayerId)


type alias PlayerName =
    String


type alias Point =
    { x : Float
    , y : Float
    }


type Image
    = UrlImage String
    | VectorImage (List (List Point))


type Appearance
    = InvisibleAppearance
    | DefaultAppearance
    | StaticImageApearance
        { front : Image
        , back : Image
        , left : Image
        , right : Image
        }


type alias GamePlayer =
    { id : Maybe PlayerId
    , name : PlayerName
    , appearance : Appearance
    , location : Location
    , direction : Direction
    }


type alias WallImage =
    { owner : PlayerName
    , image : Image
    }


type alias WallImages =
    List ( Direction, WallImage )


type alias Game =
    { board : Board
    , namePlayerDict : Dict PlayerName GamePlayer
    , locPlayersDict : Dict Location (List PlayerName)
    , wallDict : Dict Location WallImages
    }
