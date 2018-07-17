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
        , GameName
        , GamePlayer
        , Image(..)
        , PlayerName
        , Point
        , SideImages
        , Url
        , WallImage
        , WallImages
        )

import Array exposing (Array)
import Dict exposing (Dict)
import JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Direction(..)
        , Location
        )
import WebSocketFramework.Types exposing (GameId, PlayerId)


{-| (x, y)
-}
type alias Point =
    ( Float, Float )


type alias Url =
    String


{-| This will eventually include an entire SVG image package.

For now, it either a URL of an image or a list of line lists.

-}
type Image
    = UrlImage Url
    | VectorImage (List (List Point))


type alias SideImages =
    { front : List Image
    , back : List Image
    , left : List Image
    , right : List Image
    }


type Appearance
    = InvisibleAppearance
    | DefaultAppearance
    | StaticImageApearance
        { front : Image
        , back : Image
        , left : Image
        , right : Image
        }
    | VaryingAppearance SideImages


type alias PlayerName =
    String


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


type alias GameName =
    String


type alias Game =
    { name : GameName
    , board : Board
    , playerDict : Dict PlayerName GamePlayer
    , playerNamesDict : Dict Location (List PlayerName)
    , wallDict : Dict Location WallImages
    }
