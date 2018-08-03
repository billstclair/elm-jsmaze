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
import List.Extra as LE
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
    | NoGame


type alias ServerRecord =
    { playerDict : Dict PlayerId (List GamePlayer)
    , gameDict : Dict GameId (List PlayerId)
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
        PingReq { message } ->
            ( state, Just <| PongRsp { message = message } )

        LoginWithPasswordReq { userid, passwordHash } ->
            loginWithPassword userid passwordHash state

        LogoutReq { playerid } ->
            logout playerid state

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


dummyPlayer : PlayerId -> FullPlayer
dummyPlayer playerid =
    { id = playerid
    , name = "noname"
    , appearance = InvisibleAppearance
    , location = ( 0, 0 )
    , direction = North
    }


{-| To do. Validate userid/password and pull games out of backing store.

For now, this creates a new player if there is none, or returns the existing one. Just enough to make the UI work against the back-end.

-}
loginWithPassword : String -> String -> ServerState -> ( ServerState, Maybe Message )
loginWithPassword userid password state =
    let
        ( gameid, state2 ) =
            newGameid state

        ( playerid, state3 ) =
            newPlayerid state2

        state4 =
            -- Hold on to this gameid. Just there to hold on to the playerid.
            addGame gameid NoGame state3

        state5 =
            -- Hold on to this playerid. No games yet.
            addPlayer playerid
                { gameid = gameid
                , player = dummyPlayer playerid
                }
                state4
    in
    ( state5
    , Just <|
        LoginRsp
            { playerid = playerid
            , currentGame = Nothing
            , allGames = []
            }
    )


removeFromGameDict : PlayerId -> List GamePlayer -> Dict GameId (List PlayerId) -> Dict GameId (List PlayerId)
removeFromGameDict playerid gamePlayers gameDict =
    List.foldl
        (\gameid dict ->
            case Dict.get gameid dict of
                Nothing ->
                    dict

                Just playerids ->
                    case LE.remove playerid playerids of
                        [] ->
                            Dict.remove gameid dict

                        pids ->
                            Dict.insert gameid pids dict
        )
        gameDict
        (List.map .gameid gamePlayers)


logout : PlayerId -> ServerState -> ( ServerState, Maybe Message )
logout playerid state =
    case getPlayer playerid state of
        Nothing ->
            ( state
            , Just <|
                ErrorRsp
                    { error = UnknownPlayerIdError playerid
                    , message = "Unknown playerid"
                    }
            )

        Just { gameid } ->
            let
                ( gameState, gamePlayers ) =
                    case state.state of
                        Just (Server { playerDict, gameDict }) ->
                            case Dict.get playerid playerDict of
                                Nothing ->
                                    ( state.state, [] )

                                Just gamePlayers ->
                                    ( Just <|
                                        Server
                                            { playerDict =
                                                Dict.remove playerid playerDict
                                            , gameDict =
                                                removeFromGameDict
                                                    playerid
                                                    gamePlayers
                                                    gameDict
                                            }
                                    , gamePlayers
                                    )

                        _ ->
                            ( state.state, [] )
            in
            ( { state | state = gameState }
                |> removePlayer playerid
                |> removeGame gameid
            , Just <|
                LogoutRsp { players = gamePlayers }
            )
