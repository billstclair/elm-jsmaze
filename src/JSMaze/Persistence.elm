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
        , boardIdKey
        , decodePersistentThing
        , modelKey
        , playerIdKey
        , readAllBoardIds
        , readAllBoardPlayerIds
        , readThing
        , writeBoard
        , writeModel
        , writePlayer
        )

import JSMaze.Board exposing (simpleBoard)
import JSMaze.EncodeDecode
    exposing
        ( decodeBoard
        , decodeModel
        , decodePlayer
        , encodeBoard
        , encodeModel
        , encodePlayer
        )
import JSMaze.SharedTypes
    exposing
        ( Board
        , Model
        , Msg(..)
        , Player
        , SavedModel
        , currentBoardId
        , currentPlayerId
        , defaultSavedModel
        , initialPlayer
        )
import Json.Encode as JE exposing (Value)
import LocalStorage exposing (LocalStorage, getItem, listKeys, setItem)
import LocalStorage.SharedTypes exposing (Key)


modelKey : String
modelKey =
    "M:"


writeModel : Model -> LocalStorage msg -> Cmd msg
writeModel model storage =
    encodeModel model
        |> setItem storage modelKey


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


writeBoard : Board -> LocalStorage msg -> Cmd msg
writeBoard board storage =
    encodeBoard board
        |> setItem storage (boardKey board)


writePlayer : Player -> LocalStorage msg -> Cmd msg
writePlayer player storage =
    encodePlayer player
        |> setItem storage (playerKey player)


type PersistentThingType
    = PersistentBoardType
    | PersistentPlayerType
    | PersistentModelType
    | UnknownType


type PersistentThing
    = PersistentBoard Board
    | PersistentPlayer Player
    | PersistentModel SavedModel


keyType : String -> PersistentThingType
keyType string =
    if String.startsWith "B:" string then
        PersistentBoardType
    else if String.startsWith "P:" string then
        PersistentPlayerType
    else if String.startsWith "M:" string then
        PersistentModelType
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
                    Ok <| PersistentBoard simpleBoard

        PersistentPlayerType ->
            case decodePlayer value of
                Ok player ->
                    Ok <| PersistentPlayer player

                Err msg ->
                    Ok <| PersistentPlayer initialPlayer

        PersistentModelType ->
            case decodeModel value of
                Ok model ->
                    Ok <| PersistentModel model

                Err msg ->
                    Ok <| PersistentModel defaultSavedModel

        _ ->
            Err <| "Unknown key type for \"" ++ key ++ "\""
