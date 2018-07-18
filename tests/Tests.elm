module Tests exposing (all)

import Expect exposing (Expectation)
import JSMaze.Board as Board
    exposing
        ( boardToStrings
        , simpleBoard
        , simpleBoardSpec
        , stringsToBoard
        )
import JSMaze.EncodeDecode as ED
    exposing
        ( decodeBoard
        , encodeBoard
        , messageDecoder
        , messageEncoder
        , stringToValue
        , valueToString
        )
import JSMaze.GameTypes
    exposing
        ( Appearance(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GameDescription
        , GameName
        , GamePlayer
        , Image(..)
        , Message(..)
        , OwnedPlace
        , OwnedPlacement
        , PaintedWall
        , PaintedWalls
        , PlayerName
        , Point
        , SideImages
        , StaticImages
        , Url
        )
import JSMaze.SharedTypes exposing (Board, Direction(..), Location)
import Json.Decode as JD
import Test exposing (..)


{-| This runs all of your tests.

Each line is for a different result type.

-}
all : Test
all =
    Test.concat <|
        List.concat
            [ List.map doTest stringData
            , List.map doTest stringListData
            , List.map doTest boardResultData
            , List.map doMessageTest messageData
            ]


log =
    Debug.log


{-| change to True to log JSON input & output results
-}
enableLogging : Bool
enableLogging =
    False


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value
    else
        value


expectResult : Result err a -> Result err a -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (toString err) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


doTest : ( String, a, a ) -> Test
doTest ( name, was, sb ) =
    test name
        (\_ ->
            expectResult (Ok sb) (Ok was)
        )


doResultTest : ( String, Result String a, Result String a ) -> Test
doResultTest ( name, was, sb ) =
    test name
        (\_ ->
            expectResult sb was
        )


{-| Tests that return strings
-}
stringData : List ( String, String, String )
stringData =
    [ ( "1+2", "1" ++ "2", "12" )
    ]


{-| Tests that return lists of strings
-}
stringListData : List ( String, List String, List String )
stringListData =
    [ ( "stringsToBoard"
      , simpleBoardSpec |> stringsToBoard "b" |> boardToStrings
      , simpleBoardSpec
      )
    ]


{-| Tests that return Results with Boards
-}
boardResultData : List ( String, Result String Board, Result String Board )
boardResultData =
    [ ( "encodeBoard"
      , simpleBoard |> encodeBoard |> decodeBoard
      , Ok simpleBoard
      )
    ]


doMessageTest : ( String, Message ) -> Test
doMessageTest ( name, message ) =
    test name
        (\_ ->
            let
                value =
                    messageEncoder message

                res =
                    JD.decodeValue messageDecoder value
            in
            expectResult (Ok message) res
        )


messageData : List ( String, Message )
messageData =
    [ ( "PingReq", PingReq "foo" )
    , ( "PongRsp", PongRsp "bar" )
    , ( "UnknownPlayerIdError"
      , ErrorRsp
            { error = UnknownPlayerIdError "foo"
            , message = "No such player id"
            }
      )
    , ( "UnknownPlayerError"
      , ErrorRsp
            { error = UnknownPlayerError { player = "Bob", game = "Zooland" }
            , message = "Ain't nobody named Bob in Zooland"
            }
      )
    , ( "IllegalMoveError"
      , ErrorRsp
            { error =
                IllegalMoveError
                    { player =
                        { player = "Joe"
                        , game = "Zooland"
                        }
                    , location = ( 1, 2 )
                    }
            , message = "You can't go there, silly."
            }
      )
    , ( "IllegalWallLocationError"
      , ErrorRsp
            { error =
                IllegalWallLocationError
                    { player =
                        { player = "Joe"
                        , game = "Zooland"
                        }
                    , location = ( 1, 2 )
                    , direction = North
                    }
            , message = "bar"
            }
      )
    , ( "UnknownAppearanceError"
      , ErrorRsp
            { error = UnknownAppearanceError "foo"
            , message = "No such saved appearance"
            }
      )
    , ( "UnknownImageError"
      , ErrorRsp
            { error = UnknownImageError "bar"
            , message = "No such saved image"
            }
      )
    ]
