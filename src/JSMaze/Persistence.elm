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
        , decodeBoardSpec
        , decodePlayer
        , encodeBoard
        , encodeBoardSpec
        , encodePlayer
        , stringToValue
        , valueToString
        )
import JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Cell
        , Direction(..)
        , Location
        , Player
        )
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import LocalStorage exposing (LocalStorage, getItem, listKeys, setItem)


type PersistentThing
    = PersistentBoard Board
    | PersistentPlayer Player


type alias Thing =
    { thingType : String
    , value : Value
    }


encodeThing : String -> Value -> Value
encodeThing thingType value =
    JE.object
        [ ( "type", JE.string thingType )
        , ( "value", value )
        ]


thingDecoder : Decoder Thing
thingDecoder =
    JD.map2 Thing
        (JD.field "type" JD.string)
        (JD.field "value" JD.value)


encodePersistentThing : PersistentThing -> Value
encodePersistentThing thing =
    case thing of
        PersistentBoard board ->
            encodeThing "board" <| encodeBoard board

        PersistentPlayer player ->
            encodeThing "player" <| encodePlayer player


persistentThingDecoder : Decoder PersistentThing
persistentThingDecoder =
    thingDecoder |> JD.andThen thingToPersistentThing


thingToPersistentThing : Thing -> Decoder PersistentThing
thingToPersistentThing thing =
    case thing.thingType of
        "board" ->
            case decodeBoard thing.value of
                Ok board ->
                    JD.succeed <| PersistentBoard board

                Err msg ->
                    JD.fail msg

        "player" ->
            case decodePlayer thing.value of
                Ok player ->
                    JD.succeed <| PersistentPlayer player

                Err msg ->
                    JD.fail msg

        x ->
            JD.fail <| "Unknown thing type: " ++ x


decodePersistentThing : Value -> Result String PersistentThing
decodePersistentThing value =
    JD.decodeValue persistentThingDecoder value


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
    encodePersistentThing (PersistentBoard board)
        |> setItem storage (boardKey board)


writePlayer : LocalStorage msg -> Player -> Cmd msg
writePlayer storage player =
    encodePersistentThing (PersistentPlayer player)
        |> setItem storage (playerKey player)
