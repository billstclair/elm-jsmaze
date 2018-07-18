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


module JSMaze.GameTypes
    exposing
        ( Appearance(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GameDescription
        , GameName
        , GamePlayer
        , Image(..)
        , Message(..)
        , OwnedPlace
        , OwnedPlacement
        , PaintedWall
        , PaintedWalls
        , PlayerName
        , Point
        , SideImages
        , StaticImages
        , Url
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


type alias StaticImages =
    { front : Image
    , back : Image
    , left : Image
    , right : Image
    }


type Appearance
    = InvisibleAppearance
    | DefaultAppearance
    | StaticImageAppearance StaticImages
    | VaryingAppearance SideImages


type alias PlayerName =
    String


type alias FullPlayer =
    { name : PlayerName
    , appearance : Appearance
    , location : Location
    , direction : Direction
    }


type alias PaintedWall =
    { owner : PlayerName
    , location : Location
    , direction : Direction
    , image : Image
    }


type alias PaintedWalls =
    List PaintedWall


type alias GameName =
    GameId


type alias Game =
    { name : GameName
    , description : String
    , owner : PlayerName
    , board : Board
    , playerDict : Dict PlayerName FullPlayer
    , playerNamesDict : Dict Location (List PlayerName)
    , wallsDict : Dict Location PaintedWalls
    }


type alias GameDescription =
    { name : GameName
    , description : String
    , owner : PlayerName
    }


type alias GamePlayer =
    { player : PlayerName
    , game : GameName
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
    -- and ".allGames" -> List Int, and .game.<int> -> Player.
    , currentGame : GamePlayer
    , allGames : List GamePlayer
    }



{---- The wire protocol ----}


type alias OwnedPlace =
    { player : GamePlayer
    , location : Location
    }


type alias OwnedPlacement =
    { player : GamePlayer
    , location : Location
    , direction : Direction
    }


type ErrorKind
    = ValidationFailedError
    | UnknownPlayerIdError PlayerId
    | UnknownPlayerError GamePlayer
    | IllegalMoveError OwnedPlace
    | IllegalWallLocationError OwnedPlacement
    | UnknownAppearanceError String
    | UnknownImageError String


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
        , allGames : List GamePlayer
        }
      -- Logout of the current session. Do NOT exit any games.
    | LogoutReq { playerid : PlayerId }
      -- Sent to everybody, so they all know these players are now inactive.
    | LogoutRsp { players : List GamePlayer }
      -- Join an existing game as the given player.
      -- Can be either a brand new player, or a player already associated
      -- with this login and the given GameName.
    | JoinGameReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Create a brand new game, with a brand new GameName.
      -- The game is initially private, meaning you have to know its name
      -- to join it.
      --
      -- It would be nice to be able to rename a game, but I'm going to
      -- wait to see how much database effort that will take before
      -- promising it.
      -- I suppose I could have an internal, random GameId, mapped to and
      -- from the user name.
      -- Or maybe the game names shouldn't need to be unique, only their IDs.
    | NewGameReq
        { playerid : PlayerId
        , game : Game
        }
      -- Sent to the joiner only, if he didn't yet have a player in this game.
      -- Otherwise, the joiner will get a JoinGameNotificationRsp, like
      -- everybody else.
    | JoinGameRsp
        { player : GamePlayer
        , game : Game
        }
      -- Sent to all existing members of the game when someone joins.
    | JoinGameNotificationRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        }
      -- Sent to become idle, but not exit from a game.
    | LeaveReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Sent to all members of the game when a player leaves.
    | LeaveRsp { player : GamePlayer }
      -- Exit from a game and disown all of your wall images.
    | ExitReq
        { playerid : PlayerId
        , player : GamePlayer
        }
      -- Sent to all members of a game when a player exits.
    | ExitRsp { player : GamePlayer }
      -- Move location and/or change direction.
      -- It will usually be illegal to move more than one square or through a wall.
      -- This may be allowed by some future God mode.
    | MoveReq
        { playerid : PlayerId
        , player : GamePlayer
        , location : Maybe Location
        , direction : Maybe Direction
        }
      -- Sent to all members of a game after a player moves.
    | MoveRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        }
      -- Change your appearance.
    | SetApearanceReq
        { playerid : PlayerId
        , player : GamePlayer
        , appearance : Appearance
        }
      -- Sent to all members of a game when a player changes his appearance.
    | SetAppearanceRsp
        { player : GamePlayer
        , appearance : Appearance
        }
      -- Paint a wall, or change the painting if the existing painting is
      -- either yours or unowned.
      -- If `Nothing` is sent for the `image`, unpaints.
    | PaintWallReq
        { playerid : PlayerId
        , player : GamePlayer
        , location : Location
        , direction : Direction
        , image : Maybe Image
        }
      -- Sent to all members when a member paints/unpaints a wall.
    | PaintWallRsp
        { player : GamePlayer
        , location : Location
        , direction : Direction
        , image : Maybe Image
        }
      -- Make a game that you own public.
    | ListGameReq
        { playerid : PlayerId
        , player : GamePlayer
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
        { player : GamePlayer
        , isListed : Bool
        }
      -- Request a list of public games.
    | GetListedGamesReq { playerid : PlayerId }
      -- The list of public games.
    | GetListedGamesRsp { games : List GameDescription }
      -- Send a chat message to a game.
    | ChatReq
        { playerid : PlayerId
        , player : GamePlayer
        , message : String
        }
      -- Receive that chat message.
    | ChatRsp
        { player : GamePlayer
        , message : String
        }
      -- Save an appearance for lookup by ListAppearancesReq or GetAppearanceReq
    | SaveAppearanceReq
        { playerid : PlayerId
        , name : String
        , appearance : Appearance
        }
      -- Acknowledge a SaveAppearanceReq
    | SaveAppearanceRsp { name : String }
      -- Request a list of saved appearance names
    | ListAppearancesReq { playerid : PlayerId }
      -- Return that list of saved appearance names
    | ListAppearancesRsp { names : List String }
      -- Request a saved appearance
    | GetAppearanceReq
        { playerid : PlayerId
        , name : String
        }
      -- Return that saved appearance
    | GetAppearanceRsp
        { name : String
        , appearance : Appearance
        }
      -- Save an image for lookup by ListImagesReq or GetImageReq
    | SaveImageReq
        { playerid : PlayerId
        , name : String
        , image : Image
        }
      -- Acknowledge a SaveImageReq
    | SaveImageRsp { name : String }
      -- Request a list of saved appearance names
    | ListImagesReq { playerid : PlayerId }
      -- Return that list of saved appearance names
    | ListImagesRsp { names : List String }
      -- Request a saved appearance
    | GetImageReq
        { playerid : PlayerId
        , name : String
        }
      -- Return that saved appearance
    | GetImageRsp
        { name : String
        , image : Image
        }
