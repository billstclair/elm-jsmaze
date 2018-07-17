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
    { name : PlayerName
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
    , description : String
    , owner : PlayerName
    , board : Board
    , playerDict : Dict PlayerName GamePlayer
    , playerNamesDict : Dict Location (List PlayerName)
    , wallDict : Dict Location WallImages
    }


type alias GameDescription =
    { name : GameName
    , description : String
    , owner : PlayerName
    }


type alias Alias =
    { game : GameName
    , player : PlayerName
    }


{-| Stored in server database.
-}
type alias Account =
    { email : String
    , oathProvider : Maybe String
    , salt : String
    , hash : String

    -- These are stored under "<hash salt email>.currentGame"
    -- and ".allGames" -> List Int, and .game.<int> -> Alias.
    , currentGame : Alias
    , allGames : List Alias
    }



{---- The wire protocol ----}


type ErrorKind
    = ValidationFailedError
    | UnknownPlayerIdError PlayerId
    | UnknownPlayerError
        { player : PlayerName
        , game : GameName
        }
    | IllegalMoveError
        { player : PlayerName
        , game : GameName
        , location : Location
        }


type Message
    = PingReq String
    | PongRsp String
    | ErrorRsp
        { error : ErrorKind
        , message : String
        }
    | LoginWithPasswordReq { email : String, passwordHash : String }
    | LoginRsp
        { playerid : PlayerId
        , currentGame : String
        , allGames : List Alias
        }
    | Logout { playerid : PlayerId }
    | Bye
    | JoinGameReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        }
    | NewGameReq
        { playerid : PlayerId
        , player : PlayerName
        , game : Game
        }
    | JoinGameRsp
        { player : PlayerName
        , game : Game
        }
    | JoinGameNoticationRsp
        { player : PlayerName
        , game : GameName
        }
    | LeaveReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        }
    | LeaveRsp
        { player : PlayerName
        , game : GameName
        }
    | ExitReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        }
    | ExitRsp
        { player : PlayerName
        , game : GameName
        }
    | MoveReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        , location : Maybe Location
        , direction : Maybe Direction
        }
    | MoveRsp
        { player : PlayerName
        , game : GameName
        , location : Location
        , direction : Direction
        }
    | SetApearanceReq
        { player : PlayerName
        , game : GameName
        , appearance : Appearance
        }
    | SetAppearanceRsp
        { player : PlayerName
        , game : GameName
        , appearance : Appearance
        }
    | PaintWallReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        , location : Location
        , direction : Direction
        , image : Maybe WallImage
        }
    | PaintWallRsp
        { player : PlayerName
        , game : GameName
        , location : Location
        , direction : Direction
        , image : Maybe WallImage
        }
    | ListGameReq
        { playerid : PlayerId
        , player : PlayerName
        , game : GameName
        }
    | UnlistGameReq
        { playerid : PlayerId
        , game : GameName
        , switchOwnership : Maybe PlayerName
        }
    | ListGameRsp
        { player : PlayerName
        , game : GameName
        , isListed : Bool
        }
    | GetListedGamesReq { playerid : PlayerId }
    | GetListedGamesRsp { games : List GameDescription }
    | ChatReq
        { playerid : PlayerId
        , game : GameName
        , message : String
        }
    | ChatRsp
        { player : PlayerName
        , game : GameName
        , message : String
        }
