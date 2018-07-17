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
        , decodeModel
        , decodePlayer
        , encodeBoard
        , encodeBoardSpec
        , encodeModel
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
        , Layout(..)
        , Location
        , Model
        , Player
        , SavedModel
        , directionToString
        , stringToDirection
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


encodeLayout : Layout -> Value
encodeLayout layout =
    JE.string <| layoutToString layout


layoutToString : Layout -> String
layoutToString layout =
    case layout of
        TopViewLayout ->
            "TopViewLayout"

        EditingLayout ->
            "EditingLayout"

        _ ->
            "NormalLayout"


stringToLayout : String -> Layout
stringToLayout string =
    case string of
        "TopViewLayout" ->
            TopViewLayout

        "EditingLayout" ->
            EditingLayout

        _ ->
            NormalLayout


layoutDecoder : JD.Decoder Layout
layoutDecoder =
    JD.map stringToLayout JD.string


encodeModel : Model -> Value
encodeModel model =
    JE.object
        [ ( "layout", encodeLayout model.layout )
        ]


modelDecoder : JD.Decoder SavedModel
modelDecoder =
    JD.map SavedModel
        (JD.field "layout" layoutDecoder)


decodeModel : Value -> Result String SavedModel
decodeModel value =
    JD.decodeValue modelDecoder value


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
    JE.string <| directionToString direction


directionDecoder : JD.Decoder Direction
directionDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case stringToDirection s of
                    Just dir ->
                        JD.succeed dir

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
