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
        , initialBoard
        , readAllBoardIds
        , readAllBoardPlayerIds
        , readThing
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
        , Msg(..)
        , Player
        , currentBoardId
        , currentPlayerId
        )
import Json.Encode as JE exposing (Value)
import LocalStorage exposing (LocalStorage, getItem, listKeys, setItem)
import LocalStorage.SharedTypes exposing (Key)


playerKey : Player -> String
playerKey player =
    playerIdKey player.boardid player.id


playerIdKey : String -> String -> String
playerIdKey boardid playerid =
    "P:" ++ boardid ++ "/" ++ playerid


boardKey : Board -> String
boardKey board =
    boardIdKey board.id


boardIdKey : String -> String
boardIdKey id =
    "B:" ++ id


readAllBoardIds : LocalStorage msg -> Cmd msg
readAllBoardIds storage =
    listKeys storage "B:"


readAllBoardPlayerIds : LocalStorage msg -> String -> Cmd msg
readAllBoardPlayerIds storage boardid =
    listKeys storage ("P:" ++ boardid ++ "/")


readThing : LocalStorage msg -> String -> Cmd msg
readThing storage key =
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


decodePersistentThing : Key -> Value -> Result String PersistentThing
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


initialBoard : LocalStorage Msg -> Cmd Msg
initialBoard storage =
    Cmd.batch
        [ readThing storage <| boardIdKey currentBoardId
        , readThing storage <| playerIdKey currentBoardId currentPlayerId
        ]
