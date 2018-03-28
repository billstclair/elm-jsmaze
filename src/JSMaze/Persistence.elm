----------------------------------------------------------------------
--
-- Persistence.elm
-- Functions for maintaining the state of the JSMaze board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.Persistence
    exposing
        ( PersistentThing(..)
        , decodePersistentThing
        , readAllBoardIds
        , readAllBoardPlayerIds
        , readBoard
        , readPlayer
        , writeBoard
        , writePlayer
        )

import JSMaze.EncodeDecode
    exposing
        ( decodeBoard
        , decodePlayer
        , encodeBoard
        , encodePlayer
        )
import JSMaze.SharedTypes
    exposing
        ( Board
        , Player
        )
import Json.Encode as JE exposing (Value)
import LocalStorage exposing (LocalStorage, getItem, listKeys, setItem)


playerKey : Player -> String
playerKey player =
    "P:" ++ player.boardid ++ "/" ++ player.id


boardKey : Board -> String
boardKey board =
    "B:" ++ board.id


readAllBoardIds : LocalStorage msg -> Cmd msg
readAllBoardIds storage =
    listKeys storage "B:"


readAllBoardPlayerIds : LocalStorage msg -> String -> Cmd msg
readAllBoardPlayerIds storage boardid =
    listKeys storage ("P:" ++ boardid ++ "/")


readBoard : LocalStorage msg -> String -> Cmd msg
readBoard storage boardid =
    getItem storage boardid


readPlayer : LocalStorage msg -> String -> Cmd msg
readPlayer storage key =
    getItem storage key


writeBoard : LocalStorage msg -> Board -> Cmd msg
writeBoard storage board =
    encodeBoard board
        |> setItem storage (boardKey board)


writePlayer : LocalStorage msg -> Player -> Cmd msg
writePlayer storage player =
    encodePlayer player
        |> setItem storage (playerKey player)


type PersistentThingType
    = PersistentBoardType
    | PersistentPlayerType
    | UnknownType


type PersistentThing
    = PersistentBoard Board
    | PersistentPlayer Player


keyType : String -> PersistentThingType
keyType string =
    if String.startsWith "B:" string then
        PersistentBoardType
    else if String.startsWith "P:" string then
        PersistentPlayerType
    else
        UnknownType


decodePersistentThing : String -> Value -> Result String PersistentThing
decodePersistentThing key value =
    case keyType key of
        PersistentBoardType ->
            case decodeBoard value of
                Ok board ->
                    Ok <| PersistentBoard board

                Err msg ->
                    Err msg

        PersistentPlayerType ->
            case decodePlayer value of
                Ok player ->
                    Ok <| PersistentPlayer player

                Err msg ->
                    Err msg

        _ ->
            Err <| "Unknown key type for \"" ++ key ++ "\""
