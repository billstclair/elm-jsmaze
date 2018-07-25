---------------------------------------------------------------------
--
-- Interface.elm
-- Processor for JSMaze server.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.Interface
    exposing
        ( GameState(..)
        , ServerState
        , messageProcessor
        )

import Dict exposing (Dict)
import JSMaze.Board exposing (simpleBoard)
import JSMaze.Types
    exposing
        ( Appearance(..)
        , Direction(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GamePlayer
        , Message(..)
        , Msg(..)
        )
import Task
import WebSocketFramework.ServerInterface
    exposing
        ( addGame
        , addPlayer
        , getGame
        , getGamePlayers
        , getPlayer
        , newGameid
        , newPlayerid
        , removeGame
        , removePlayer
        , updateGame
        , updatePlayer
        )
import WebSocketFramework.Types as Types
    exposing
        ( GameId
        , PlayerId
        , Plist
        , PublicGame
        , ReqRsp(..)
        )


{-| TODO
-}
type GameState
    = Game Game
    | Server ServerRecord


type alias ServerRecord =
    { playerDict : Dict PlayerId (List GamePlayer)
    }


type alias ServerState =
    Types.ServerState GameState FullPlayer


type alias TaskToCmdMessage =
    Message -> Message


{-| `messageProcessorInternal` can return a `TaskReq` message.

When it does, this function turns that into a `CmdReq` message.

The top-level `update` function, will return the encapsulated `Cmd` for
processing by the runtime, instead of sending the CmdReq back to the client.

-}
taskToCmdMessage : TaskToCmdMessage
taskToCmdMessage message =
    case message of
        TaskReq { task } ->
            CmdReq { cmd = Task.attempt ReceiveTask task }

        _ ->
            message


messageProcessor : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessor state message =
    let
        ( s, mm ) =
            messageProcessorInternal state message
    in
    case mm of
        Nothing ->
            ( s, mm )

        Just m ->
            ( s, Just <| taskToCmdMessage m )


messageProcessorInternal : ServerState -> Message -> ( ServerState, Maybe Message )
messageProcessorInternal state message =
    case message of
        LoginWithPasswordReq { userid, passwordHash } ->
            loginWithPassword userid passwordHash state

        _ ->
            ( state
            , Just <|
                ErrorRsp
                    { error = RandomError "Not implemented"
                    , message = "Message not yet implemented: " ++ toString message
                    }
            )


dummyGameName : String
dummyGameName =
    "Maze"


{-| To do. Validate and pull data out of backing store.

For now, this creates a new player if there is none, or returns the existing one. Just enough to make the UI work against the back-end.

-}
loginWithPassword : String -> String -> ServerState -> ( ServerState, Maybe Message )
loginWithPassword userid password state =
    case state.state of
        Just (Server { playerDict }) ->
            case Dict.toList playerDict of
                ( playerid, playerGames ) :: _ ->
                    -- TODO
                    ( state, Nothing )

                _ ->
                    newPlayer state

        _ ->
            newPlayer state


newPlayer : ServerState -> ( ServerState, Maybe Message )
newPlayer state =
    let
        gameName =
            dummyGameName

        playerName =
            "Player"

        ( gameid, state2 ) =
            newGameid state

        gamePlayer =
            { player = playerName
            , game = gameid
            }

        ( playerid, state3 ) =
            newPlayerid state2

        playerGames =
            [ gamePlayer ]

        ( serverRecord, state4 ) =
            case state.state of
                Just (Server serverRecord) ->
                    ( serverRecord, state2 )

                _ ->
                    let
                        serverRecord =
                            { playerDict = Dict.empty }
                    in
                    ( serverRecord
                    , { state3
                        | state = Just (Server serverRecord)
                      }
                    )

        player =
            { id = playerid
            , name = playerName
            , appearance = DefaultAppearance
            , location = ( 0, 0 )
            , direction = South
            }

        game =
            { id = gameid
            , name = gameName
            , description = "The default maze."
            , owner = playerName
            , board = simpleBoard
            , playerDict =
                Dict.fromList
                    [ ( playerName, player ) ]
            , playerNamesDict =
                Dict.fromList
                    [ ( player.location, [ playerName ] ) ]
            , wallsDict = Dict.empty
            }

        state5 =
            addGame gameid (Game game) state4

        playerInfo =
            { gameid = gameid
            , player = player
            }

        state6 =
            addPlayer playerid playerInfo state5
    in
    ( state6, Nothing )
