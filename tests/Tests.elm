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
        ( boardEncoder
        , decodeBoard
        , messageDecoder
        , messageEncoder
        , stringToValue
        , valueToString
        )
import JSMaze.SharedTypes
    exposing
        ( Appearance(..)
        , Board
        , Direction(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GameDescription
        , GameName
        , GamePlayer
        , Image(..)
        , Location
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
import Json.Decode as JD
import Test exposing (..)
import WebSocketFramework.EncodeDecode
    exposing
        ( decodeMessage
        , encodeMessage
        )


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
            , testMap protocolTest protocolData
            ]


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


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
    [ ( "boardEncoder"
      , simpleBoard |> boardEncoder |> decodeBoard
      , Ok simpleBoard
      )
    ]


encode : Message -> String
encode message =
    encodeMessage messageEncoder message


decode : String -> Result String Message
decode string =
    decodeMessage messageDecoder string


protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
            let
                json =
                    maybeLog "protocolJson" <| encode message
            in
            expectResult (Ok message) <| decode json
        )


protocolData : List Message
protocolData =
    [ PingReq { message = "foo" }
    , PongRsp { message = "bar" }
    , ErrorRsp
        { error = UnknownPlayerIdError "foo"
        , message = "No such player id"
        }
    , ErrorRsp
        { error = UnknownPlayerError { player = "Bob", game = "Zooland" }
        , message = "Ain't nobody named Bob in Zooland"
        }
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
    , ErrorRsp
        { error = UnknownAppearanceError "foo"
        , message = "No such saved appearance"
        }
    , ErrorRsp
        { error = UnknownImageError "bar"
        , message = "No such saved image"
        }
    , LoginWithPasswordReq
        { email = "nobody@nowhere.com"
        , passwordHash = "what, me worry?"
        }
    , LoginRsp
        { playerid = "player"
        , currentGame = "game"
        , allGames =
            [ { player = "player", game = "game" }
            , { player = "player2", game = "game2" }
            ]
        }
    , LogoutReq { playerid = "player" }
    , LogoutRsp
        { players = [ player1, player2 ] }
    , JoinGameReq
        { playerid = "player"
        , player = player1
        }
    , NewGameReq
        { playerid = "player"
        , game = game1
        }
    , JoinGameRsp
        { player = player1
        , game = game1
        }
    , JoinGameNotificationRsp
        { player = player1
        , location = ( 0, 0 )
        , direction = South
        }
    , LeaveReq
        { playerid = "player"
        , player = player1
        }
    , LeaveRsp { player = player1 }
    , ExitReq
        { playerid = "player"
        , player = player1
        }
    , ExitRsp { player = player1 }
    , MoveReq
        { playerid = "player"
        , player = player1
        , location = Just ( 1, 2 )
        , direction = Nothing
        }
    , MoveReq
        { playerid = "player"
        , player = player2
        , location = Nothing
        , direction = Just North
        }
    , MoveRsp
        { player = player2
        , location = ( 1, 2 )
        , direction = North
        }
    ]


player1 : GamePlayer
player1 =
    { player = "player", game = "game" }


player2 : GamePlayer
player2 =
    { player = "player2", game = "game2" }


game1 : Game
game1 =
    { name = "Bill's Maze"
    , description = "Just a little maze."
    , owner = "Bill"
    , board = simpleBoard
    , playerDict =
        Dict.fromList
            [ ( "Bill", fullPlayer1 )
            , ( "Moe", fullPlayer2 )
            , ( "Larry", fullPlayer3 )
            , ( "Curly", fullPlayer4 )
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


fullPlayer1 : FullPlayer
fullPlayer1 =
    { name = "Bill"
    , appearance = InvisibleAppearance
    , location = ( 0, 0 )
    , direction = North
    }


fullPlayer2 : FullPlayer
fullPlayer2 =
    { name = "Moe"
    , appearance = DefaultAppearance
    , location = ( 0, 0 )
    , direction = South
    }


fullPlayer3 : FullPlayer
fullPlayer3 =
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


fullPlayer4 : FullPlayer
fullPlayer4 =
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
