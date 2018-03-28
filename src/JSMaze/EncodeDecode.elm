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
        , encodeBoard
        , encodeBoardSpec
        , stringToValue
        , valueToString
        )

import JSMaze.Board exposing (boardToStrings, setId, stringsToBoard)
import JSMaze.SharedTypes exposing (Board, BoardSpec)
import Json.Decode as JD
import Json.Encode as JE exposing (Value)


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
        |> JD.map stringsToBoard


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


valueToString : Value -> String
valueToString value =
    JE.encode 0 value


stringToValue : String -> Value
stringToValue string =
    JD.decodeString JD.value string
        |> Result.withDefault JE.null
