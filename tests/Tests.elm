module Tests exposing (all)

import Dict exposing (Dict)
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
    , ( "LoginWithPasswordReq"
      , LoginWithPasswordReq
            { email = "nobody@nowhere.com"
            , passwordHash = "what, me worry?"
            }
      )
    , ( "LoginRsp"
      , LoginRsp
            { playerid = "player"
            , currentGame = "game"
            , allGames =
                [ { player = "player", game = "game" }
                , { player = "player2", game = "game2" }
                ]
            }
      )
    , ( "LogoutReq"
      , LogoutReq
            { playerid = "player" }
      )
    , ( "LogoutRsp"
      , LogoutRsp
            { players =
                [ { player = "player", game = "game" }
                , { player = "player2", game = "game2" }
                ]
            }
      )
    , ( "JoinGameReq"
      , JoinGameReq
            { playerid = "player"
            , player = { player = "player", game = "game" }
            }
      )
    , ( "NewGameReq"
      , NewGameReq
            { playerid = "player"
            , game =
                { name = "Bill's Maze"
                , description = "Just a little maze."
                , owner = "Bill"
                , board = simpleBoard
                , playerDict =
                    Dict.fromList
                        [ ( "Bill", player1 )
                        , ( "Moe", player2 )
                        , ( "Larry", player3 )
                        , ( "Curly", player4 )
                        ]
                , playerNamesDict =
                    Dict.fromList
                        -- The lists must be in alphabetical order here,
                        -- or the test will fail, even though it's logically OK.
                        [ ( ( 0, 0 ), [ "Bill", "Moe" ] )
                        , ( ( 1, 2 ), [ "Curly", "Larry" ] )
                        ]
                , wallsDict =
                    Dict.fromList
                        [ ( ( 0, 0 ), [ wall1 ] )
                        , ( ( 1, 1 ), [ wall2, wall3 ] )
                        ]
                }
            }
      )
    ]


player1 : FullPlayer
player1 =
    { name = "Bill"
    , appearance = InvisibleAppearance
    , location = ( 0, 0 )
    , direction = North
    }


player2 : FullPlayer
player2 =
    { name = "Moe"
    , appearance = DefaultAppearance
    , location = ( 0, 0 )
    , direction = South
    }


player3 : FullPlayer
player3 =
    { name = "Larry"
    , appearance =
        StaticImageAppearance
            { front = image1
            , back = image2
            , left = image3
            , right = image4
            }
    , location = ( 1, 2 )
    , direction = East
    }


player4 : FullPlayer
player4 =
    { name = "Curly"
    , appearance =
        VaryingAppearance
            { front = [ image1 ]
            , back = [ image1, image2 ]
            , left = [ image1, image2, image3 ]
            , right = [ image1, image2, image3, image4 ]
            }
    , location = ( 1, 2 )
    , direction = East
    }


image1 : Image
image1 =
    UrlImage "front.jpg"


image2 : Image
image2 =
    UrlImage "back.jpg"


image3 : Image
image3 =
    VectorImage
        [ [ ( 0, 0 ), ( 0, 1 ) ]
        , [ ( 1, 1 ), ( 1, 0 ), ( 0, 1 ) ]
        ]


image4 : Image
image4 =
    VectorImage
        [ [ ( 1, 1 ), ( 1, 0 ), ( 0, 1 ) ]
        , [ ( 0, 0 ), ( 0, 1 ) ]
        ]


wall1 : PaintedWall
wall1 =
    { owner = "Bill"
    , location = ( 0, 0 )
    , direction = West
    , image = image1
    }


wall2 : PaintedWall
wall2 =
    { owner = "Moe"
    , location = ( 1, 1 )
    , direction = East
    , image = image3
    }


wall3 : PaintedWall
wall3 =
    { owner = "Larry"
    , location = ( 1, 1 )
    , direction = West
    , image = image4
    }
