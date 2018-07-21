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
        ( Appearance(..)
        , Board
        , BoardSpec
        , Cell
        , Direction(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GameDescription
        , GameName
        , GamePlayer
        , Image(..)
        , Layout(..)
        , Location
        , Message(..)
        , Model
        , Msg(..)
        , Operation(..)
        , OwnedPlace
        , OwnedPlacement
        , PaintedWall
        , PaintedWalls
        , Player
        , PlayerName
        , Point
        , Row
        , SavedModel
        , SideImages
        , StaticImages
        , Url
        , WallSpec
        , Walls
        , currentBoardId
        , currentPlayerId
        , defaultSavedModel
        , directionToString
        , initialPlayer
        , operationToDirection
        , stringToDirection
        , sumLocations
        )

import Array exposing (Array)
import Dict exposing (Dict)
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes as LST exposing (Key, Ports, Value)
import Svg.Button as Button exposing (Button)
import Task exposing (Task)
import Time exposing (Time)
import WebSocketFramework.Types exposing (GameId, PlayerId)
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
    | ReceiveTask (Result String Message)
    | Nop


type Direction
    = North
    | South
    | East
    | West


directionToString : Direction -> String
directionToString direction =
    case direction of
        North ->
            "north"

        South ->
            "south"

        East ->
            "east"

        West ->
            "west"


stringToDirection : String -> Maybe Direction
stringToDirection string =
    case string of
        "north" ->
            Just North

        "south" ->
            Just South

        "east" ->
            Just East

        "west" ->
            Just West

        _ ->
            Nothing


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



{--- Types for the server, and the wire. ---}


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
    = PingReq { message : String }
    | PongRsp { message : String }
    | TaskReq { task : Task String Message }
    | CmdReq { cmd : Cmd Msg }
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
        , currentGame : GameName
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
      -- The game.owner will be auto-joined, so the response will be
      -- a JoinGameNotificationRsp, not a JoinGameRsp.
      --
      -- It would be nice to be able to rename a game, but I'm going to
      -- wait to see how much database effort that will take before
      -- promising it.
      -- I suppose I could have an internal, random GameId, mapped to and
      -- from the user name.
      -- Then the game names wouldn't need to be unique, only their IDs.
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
