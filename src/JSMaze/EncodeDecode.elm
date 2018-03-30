----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Functions for maintaining the state of the JSMaze board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.EncodeDecode
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

import JSMaze.Board exposing (boardToStrings, setId, stringsToBoard)
import JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Direction(..)
        , Location
        , Player
        )
import Json.Decode as JD
import Json.Encode as JE exposing (Value)


valueToString : Value -> String
valueToString value =
    JE.encode 0 value


stringToValue : String -> Value
stringToValue string =
    JD.decodeString JD.value string
        |> Result.withDefault JE.null


encodeBoard : Board -> Value
encodeBoard board =
    JE.object
        [ ( "id", JE.string board.id )
        , ( "spec", encodeBoardSpec board )
        ]


encodeBoardSpec : Board -> Value
encodeBoardSpec board =
    boardToStrings board
        |> List.map JE.string
        |> JE.list


boardSpecDecoder : JD.Decoder Board
boardSpecDecoder =
    JD.list
        JD.string
        |> JD.map (stringsToBoard "")


boardDecoder : JD.Decoder Board
boardDecoder =
    JD.map2 setId
        (JD.field "id" JD.string)
        (JD.field "spec" boardSpecDecoder)


decodeBoard : Value -> Result String Board
decodeBoard value =
    JD.decodeValue boardDecoder value


decodeBoardSpec : Value -> Result String Board
decodeBoardSpec value =
    JD.decodeValue boardSpecDecoder value


encodeLocation : Location -> Value
encodeLocation ( x, y ) =
    JE.object
        [ ( "x", JE.int x )
        , ( "y", JE.int y )
        ]


locationDecoder : JD.Decoder Location
locationDecoder =
    JD.map2 (,)
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)


encodeDirection : Direction -> Value
encodeDirection direction =
    case direction of
        North ->
            JE.string "north"

        South ->
            JE.string "south"

        East ->
            JE.string "east"

        West ->
            JE.string "west"


directionDecoder : JD.Decoder Direction
directionDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "north" ->
                        JD.succeed North

                    "south" ->
                        JD.succeed South

                    "east" ->
                        JD.succeed East

                    "west" ->
                        JD.succeed West

                    _ ->
                        JD.fail "Unknown Direction"
            )


encodePlayer : Player -> Value
encodePlayer player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "boardid", JE.string player.boardid )
        , ( "name", JE.string player.name )
        , ( "location", encodeLocation player.location )
        , ( "direction", encodeDirection player.direction )
        ]


playerDecoder : JD.Decoder Player
playerDecoder =
    JD.map5 Player
        (JD.field "id" JD.string)
        (JD.field "boardid" JD.string)
        (JD.field "name" JD.string)
        (JD.field "location" locationDecoder)
        (JD.field "direction" directionDecoder)


decodePlayer : Value -> Result String Player
decodePlayer value =
    JD.decodeValue playerDecoder value
