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

The salt is mixed with the `passwordHash` in the `LoginWithPasswordReq`
Message, and hashed to the `hash`.

-}
type alias Account =
    { email : String
    , oauthProvider : Maybe String
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


type alias Player =
    { player : PlayerName
    , game : GameName
    }


type
    Message
    -- A request for a pong, just to ensure the server is up.
    = PingReq String
    | PongRsp String
      -- Returned when an error occurs
    | ErrorRsp
        { error : ErrorKind
        , message : String
        }
      -- When I figure out how to do it, there will also be
      -- a way to login from an oauthProvier
      -- passwordHash is a simple hash of the user-typed password
    | LoginWithPasswordReq { email : String, passwordHash : String }
      -- Sent to only the requester
    | LoginRsp
        { playerid : PlayerId
        , currentGame : String
        , allGames : List Alias
        }
      -- Logout of the current session. Do NOT exit any games.
    | LogoutReq { playerid : PlayerId }
      -- Sent to everybody, so they all know these players are now inactive.
    | LogoutRsp { players : List Player }
      -- Join an existing game as the given player.
      -- Can be either a brand new player, or a player already associated
      -- with this login and the given GameName.
    | JoinGameReq
        { playerid : PlayerId
        , player : Player
        }
      -- Create a brand new game, with a brand new GameName.
      -- The game is initially private, meaning you have to know its name
      -- to join it.
    | NewGameReq
        { playerid : PlayerId
        , game : Game
        }
      -- Sent to the joiner only doesn't yet have a player in this game.
    | JoinGameRsp
        { player : Player
        , game : Game
        }
      -- Sent to all existing members of the game when someone joins.
    | JoinGameNotificationRsp
        { player : Player
        , location : Location
        , direction : Direction
        }
      -- Sent to become idle, but not exist from a game.
    | LeaveReq
        { playerid : PlayerId
        , player : Player
        }
      -- Sent to all members of the game when a player leaves.
    | LeaveRsp { player : Player }
      -- Exit from a game and disown all of your wall images.
    | ExitReq
        { playerid : PlayerId
        , player : Player
        }
      -- Sent to all members of a game when a player exits.
    | ExitRsp { player : Player }
      -- Move location and/or change direction.
      -- It will usually be illegal to move more than one square or through a wall.
      -- This may be allowed by some future God mode.
    | MoveReq
        { playerid : PlayerId
        , player : Player
        , location : Maybe Location
        , direction : Maybe Direction
        }
      -- Sent to all members of a game after a player moves.
    | MoveRsp
        { player : Player
        , location : Location
        , direction : Direction
        }
      -- Change your appearance.
    | SetApearanceReq
        { playerid : PlayerId
        , player : Player
        , appearance : Appearance
        }
      -- Sent to all members of a game when a player changes his appearance.
    | SetAppearanceRsp
        { player : Player
        , appearance : Appearance
        }
      -- Paint a wall, or change the painting if the existing painting is
      -- either yours or unowned.
      -- If `Nothing` is sent for the `image`, unpaints.
    | PaintWallReq
        { playerid : PlayerId
        , player : Player
        , location : Location
        , direction : Direction
        , image : Maybe WallImage
        }
      -- Sent to all members when a member paints/unpaints a wall.
    | PaintWallRsp
        { player : Player
        , location : Location
        , direction : Direction
        , image : Maybe WallImage
        }
      -- Make a game that you own public.
    | ListGameReq
        { playerid : PlayerId
        , player : Player
        }
      -- Make a game that you own no longer public.
      -- Optionally switch its ownership.
      -- If switchOwnership is `Just ""`, make the game unowned.
      -- Otherwise, a new owner must be a member.
      -- As soon as all members exit a private game, it will be destroyed.
    | UnlistGameReq
        { playerid : PlayerId
        , game : GameName
        , switchOwnership : Maybe PlayerName
        }
      -- Sent to all members when a game is listed or unlisted.
    | ListGameRsp
        { player : Player
        , isListed : Bool
        }
      -- Request a list of public games.
    | GetListedGamesReq { playerid : PlayerId }
      -- The list of public games.
    | GetListedGamesRsp { games : List GameDescription }
      -- Send a chat message to a game.
    | ChatReq
        { playerid : PlayerId
        , game : GameName
        , message : String
        }
      -- Receive that chat message.
    | ChatRsp
        { player : Player
        , message : String
        }
