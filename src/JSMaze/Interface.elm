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


module JSMaze.Interface exposing (..)

import JSMaze.Types
    exposing
        ( Message(..)
        , Msg(..)
        )
import Task
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
type alias Player =
    String


{-| TODO
-}
type alias GameState =
    ()


type alias ServerState =
    Types.ServerState GameState Player


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
    ( state, Just message )
