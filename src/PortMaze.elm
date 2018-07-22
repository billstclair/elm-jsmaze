----------------------------------------------------------------------
--
-- PortMaze.elm
-- Top-level for port-based site application.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


port module PortMaze exposing (..)

import Html
import JSMaze exposing (init, update, view)
import JSMaze.Types exposing (Model, Msg(..))
import LocalStorage
import LocalStorage.SharedTypes
    exposing
        ( ClearPort
        , GetItemPort
        , ListKeysPort
        , Operation(..)
        , Ports
        , ReceiveItemPort
        , SetItemPort
        , receiveWrapper
        )


main =
    Html.programWithFlags
        { init = \initialValue -> init initialValue (Just ports)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


ports : Ports Msg
ports =
    LocalStorage.makeRealPorts getItem setItem clear listKeys


port getItem : GetItemPort msg


port setItem : SetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port receiveItem : ReceiveItemPort msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        prefix =
            LocalStorage.getPrefix model.storage
    in
    Sub.batch
        [ receiveItem <| receiveWrapper UpdatePorts prefix
        , JSMaze.subscriptions model
        ]
