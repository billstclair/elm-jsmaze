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
import JSMaze.GameTypes
    exposing
        ( Appearance(..)
        , Game
        , GameName
        , GamePlayer
        , Image(..)
        , Message(..)
        , PlayerName
        , Point
        , SideImages
        , StaticImages
        , Url
        , WallImage
        , WallImages
        )
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
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import WebSocketFramework exposing (decodePlist, unknownMessage)
import WebSocketFramework.EncodeDecode exposing (genericMessageDecoder)
import WebSocketFramework.ServerInterface as ServerInterface
import WebSocketFramework.Types
    exposing
        ( DecoderPlist
        , GameId
        , MessageDecoder
        , MessageEncoder
        , Plist
        , PublicGame
        , ReqRsp(..)
        , ServerUrl
        )


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


layoutDecoder : Decoder Layout
layoutDecoder =
    JD.map stringToLayout JD.string


encodeModel : Model -> Value
encodeModel model =
    JE.object
        [ ( "layout", encodeLayout model.layout )
        ]


modelDecoder : Decoder SavedModel
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


boardSpecDecoder : Decoder Board
boardSpecDecoder =
    JD.list
        JD.string
        |> JD.map (stringsToBoard "")


boardDecoder : Decoder Board
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


locationDecoder : Decoder Location
locationDecoder =
    JD.map2 (,)
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)


encodeDirection : Direction -> Value
encodeDirection direction =
    JE.string <| directionToString direction


directionDecoder : Decoder Direction
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


playerDecoder : Decoder Player
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



{---- Message coding ----}


pointEncoder : Point -> Value
pointEncoder ( x, y ) =
    JE.list [ JE.float x, JE.float y ]


pointDecoder : Decoder Point
pointDecoder =
    JD.list JD.float
        |> JD.andThen
            (\p ->
                case p of
                    [ x, y ] ->
                        JD.succeed ( x, y )

                    _ ->
                        JD.fail "Malformed point"
            )


imageEncoder : Image -> Value
imageEncoder image =
    case image of
        UrlImage url ->
            JE.object
                [ ( "UrlImage", JE.string url )
                ]

        VectorImage pointLists ->
            JE.object
                [ ( "VectorImage"
                  , JE.list
                        (List.map
                            (\points ->
                                JE.list <|
                                    List.map pointEncoder points
                            )
                            pointLists
                        )
                  )
                ]


imageDecoder : Decoder Image
imageDecoder =
    JD.oneOf
        [ JD.map UrlImage <|
            JD.field "UrlImage" JD.string
        , JD.map VectorImage <|
            JD.field "VectorImage" (JD.list (JD.list pointDecoder))
        ]


sideImagesEncoder : SideImages -> Value
sideImagesEncoder images =
    JE.object
        [ ( "front", JE.list <| List.map imageEncoder images.front )
        , ( "back", JE.list <| List.map imageEncoder images.back )
        , ( "left", JE.list <| List.map imageEncoder images.left )
        , ( "right", JE.list <| List.map imageEncoder images.right )
        ]


sideImagesDecoder : Decoder SideImages
sideImagesDecoder =
    decode SideImages
        |> required "front" (JD.list imageDecoder)
        |> required "back" (JD.list imageDecoder)
        |> required "left" (JD.list imageDecoder)
        |> required "right" (JD.list imageDecoder)


staticImagesEncoder : StaticImages -> Value
staticImagesEncoder { front, back, left, right } =
    JE.object
        [ ( "front", imageEncoder front )
        , ( "back", imageEncoder back )
        , ( "left", imageEncoder left )
        , ( "right", imageEncoder right )
        ]


staticImagesDecoder : Decoder StaticImages
staticImagesDecoder =
    decode StaticImages
        |> required "front" imageDecoder
        |> required "back" imageDecoder
        |> required "left" imageDecoder
        |> required "right" imageDecoder


appearanceEncoder : Appearance -> Value
appearanceEncoder appearance =
    case appearance of
        InvisibleAppearance ->
            JE.object [ ( "InvisibleAppearance", JE.null ) ]

        DefaultAppearance ->
            JE.object [ ( "DefaultAppearance", JE.null ) ]

        StaticImageAppearance staticImages ->
            JE.object
                [ ( "StaticImageAppearance", staticImagesEncoder staticImages ) ]

        VaryingAppearance sideImages ->
            JE.object [ ( "VaryingAppearance", sideImagesEncoder sideImages ) ]


appearanceDecoder : Decoder Appearance
appearanceDecoder =
    JD.oneOf
        [ JD.map (\_ -> InvisibleAppearance) <|
            JD.field "InvisibleAppearance" JD.value
        , JD.map (\_ -> DefaultAppearance) <|
            JD.field "DefaultAppearance" JD.value
        , JD.map StaticImageAppearance <|
            JD.field "StaticImageAppearance" staticImagesDecoder
        , JD.map VaryingAppearance <|
            JD.field "VaryingAppearance" sideImagesDecoder
        ]
