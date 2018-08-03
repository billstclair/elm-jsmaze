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
        ( boardEncoder
        , boardSpecEncoder
        , decodeBoard
        , decodeBoardSpec
        , decodeModel
        , decodePlayer
        , messageDecoder
        , messageEncoder
        , modelEncoder
        , playerEncoder
        , stringToValue
        , valueToString
        )

import Dict exposing (Dict)
import JSMaze.Board exposing (boardToStrings, setId, stringsToBoard)
import JSMaze.Types
    exposing
        ( Appearance(..)
        , Board
        , BoardSpec
        , Direction(..)
        , ErrorKind(..)
        , FullPlayer
        , Game
        , GameDescription
        , GameName
        , GamePlayer
        , Image(..)
        , Layout(..)
        , Location
        , Message(..)
        , Model
        , OwnedPlace
        , OwnedPlacement
        , PaintedWall
        , Player
        , PlayerName
        , Point
        , SavedModel
        , SideImages
        , StaticImages
        , Url
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


layoutEncoder : Layout -> Value
layoutEncoder layout =
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


modelEncoder : Model -> Value
modelEncoder model =
    JE.object
        [ ( "layout", layoutEncoder model.layout )
        ]


modelDecoder : Decoder SavedModel
modelDecoder =
    JD.map SavedModel
        (JD.field "layout" layoutDecoder)


decodeModel : Value -> Result String SavedModel
decodeModel value =
    JD.decodeValue modelDecoder value


boardEncoder : Board -> Value
boardEncoder board =
    JE.object
        [ ( "id", JE.string board.id )
        , ( "spec", boardSpecEncoder board )
        ]


boardSpecEncoder : Board -> Value
boardSpecEncoder board =
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


locationEncoder : Location -> Value
locationEncoder ( x, y ) =
    JE.object
        [ ( "x", JE.int x )
        , ( "y", JE.int y )
        ]


locationDecoder : Decoder Location
locationDecoder =
    JD.map2 (,)
        (JD.field "x" JD.int)
        (JD.field "y" JD.int)


directionEncoder : Direction -> Value
directionEncoder direction =
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


playerEncoder : Player -> Value
playerEncoder player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "boardid", JE.string player.boardid )
        , ( "name", JE.string player.name )
        , ( "location", locationEncoder player.location )
        , ( "direction", directionEncoder player.direction )
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


fullPlayerEncoder : FullPlayer -> Value
fullPlayerEncoder player =
    JE.object
        [ ( "id", JE.string player.id )
        , ( "name", JE.string player.name )
        , ( "appearance", appearanceEncoder player.appearance )
        , ( "location", locationEncoder player.location )
        , ( "direction", directionEncoder player.direction )
        ]


fullPlayerDecoder : Decoder FullPlayer
fullPlayerDecoder =
    decode FullPlayer
        |> required "id" JD.string
        |> required "name" JD.string
        |> required "appearance" appearanceDecoder
        |> required "location" locationDecoder
        |> required "direction" directionDecoder


paintedWallEncoder : PaintedWall -> Value
paintedWallEncoder image =
    JE.object
        [ ( "owner", JE.string image.owner )
        , ( "location", locationEncoder image.location )
        , ( "direction", directionEncoder image.direction )
        , ( "image", imageEncoder image.image )
        ]


paintedWallDecoder : Decoder PaintedWall
paintedWallDecoder =
    decode PaintedWall
        |> required "owner" JD.string
        |> required "location" locationDecoder
        |> required "direction" directionDecoder
        |> required "image" imageDecoder


{-| We'll need another encoder for the persistent store.

It will store the dicts as separately-indexed items, to reduce bandwidth.

-}
gameEncoder : Game -> Value
gameEncoder game =
    JE.object
        [ ( "id", JE.string game.id )
        , ( "name", JE.string game.name )
        , ( "description", JE.string game.description )
        , ( "owner", JE.string game.owner )
        , ( "board", boardEncoder game.board )
        , ( "players"
          , JE.list
                (List.map fullPlayerEncoder <|
                    Dict.values game.playerDict
                )
          )
        , ( "walls"
          , JE.list
                (List.map paintedWallEncoder <|
                    List.concat (Dict.values game.wallsDict)
                )
          )
        ]


makeGame : String -> String -> String -> String -> Board -> List FullPlayer -> List PaintedWall -> Game
makeGame id name description owner board players walls =
    let
        playerNamesDict =
            List.foldr
                (\player dict ->
                    Dict.insert player.location
                        (case Dict.get player.location dict of
                            Nothing ->
                                [ player.name ]

                            Just players ->
                                player.name :: players
                        )
                        dict
                )
                Dict.empty
                players

        wallsDict =
            List.foldr
                (\wall dict ->
                    Dict.insert wall.location
                        (case Dict.get wall.location dict of
                            Nothing ->
                                [ wall ]

                            Just walls ->
                                wall :: walls
                        )
                        dict
                )
                Dict.empty
                walls
    in
    { id = id
    , name = name
    , description = description
    , owner = owner
    , board = board
    , playerDict =
        Dict.fromList <|
            List.map (\player -> ( player.name, player )) players
    , playerNamesDict = playerNamesDict
    , wallsDict = wallsDict
    }


gameDecoder : Decoder Game
gameDecoder =
    decode makeGame
        |> required "id" JD.string
        |> required "name" JD.string
        |> required "description" JD.string
        |> required "owner" JD.string
        |> required "board" boardDecoder
        |> required "players" (JD.list fullPlayerDecoder)
        |> required "walls" (JD.list paintedWallDecoder)


gameDescriptionEncoder : GameDescription -> Value
gameDescriptionEncoder description =
    JE.object
        [ ( "name", JE.string description.name )
        , ( "description", JE.string description.description )
        , ( "owner", JE.string description.owner )
        ]


gameDescriptionDecoder : Decoder GameDescription
gameDescriptionDecoder =
    decode GameDescription
        |> required "name" JD.string
        |> required "description" JD.string
        |> required "owner" JD.string


gamePlayerEncoder : GamePlayer -> Value
gamePlayerEncoder player =
    JE.object
        [ ( "player", JE.string player.player )
        , ( "gameid", JE.string player.gameid )
        ]


gamePlayerDecoder : Decoder GamePlayer
gamePlayerDecoder =
    decode GamePlayer
        |> required "player" JD.string
        |> required "gameid" JD.string


ownedPlaceEncoder : OwnedPlace -> Value
ownedPlaceEncoder place =
    JE.object
        [ ( "player", gamePlayerEncoder place.player )
        , ( "location", locationEncoder place.location )
        ]


ownedPlaceDecoder : Decoder OwnedPlace
ownedPlaceDecoder =
    decode OwnedPlace
        |> required "player" gamePlayerDecoder
        |> required "location" locationDecoder


ownedPlacementEncoder : OwnedPlacement -> Value
ownedPlacementEncoder place =
    JE.object
        [ ( "player", gamePlayerEncoder place.player )
        , ( "location", locationEncoder place.location )
        , ( "direction", directionEncoder place.direction )
        ]


ownedPlacementDecoder : Decoder OwnedPlacement
ownedPlacementDecoder =
    decode OwnedPlacement
        |> required "player" gamePlayerDecoder
        |> required "location" locationDecoder
        |> required "direction" directionDecoder


errorKindEncoder : ErrorKind -> Value
errorKindEncoder kind =
    case kind of
        ValidationFailedError ->
            JE.object [ ( "ValidationFailedError", JE.null ) ]

        UnknownPlayerIdError playerid ->
            JE.object [ ( "UnknownPlayerIdError", JE.string playerid ) ]

        UnknownPlayerError player ->
            JE.object [ ( "UnknownPlayerError", gamePlayerEncoder player ) ]

        IllegalMoveError place ->
            JE.object [ ( "IllegalMoveError", ownedPlaceEncoder place ) ]

        IllegalWallLocationError placement ->
            JE.object
                [ ( "IllegalWallLocationError"
                  , ownedPlacementEncoder placement
                  )
                ]

        UnknownAppearanceError appearanceName ->
            JE.object [ ( "UnknownAppearanceError", JE.string appearanceName ) ]

        UnknownImageError imageName ->
            JE.object [ ( "UnknownImageError", JE.string imageName ) ]

        RandomError message ->
            JE.object [ ( "RandomError", JE.string message ) ]


errorKindDecoder : Decoder ErrorKind
errorKindDecoder =
    JD.oneOf
        [ JD.map (\_ -> ValidationFailedError) <|
            JD.field "ValidationFailedError" JD.value
        , JD.map UnknownPlayerIdError <|
            JD.field "UnknownPlayerIdError" JD.string
        , JD.map UnknownPlayerError <|
            JD.field "UnknownPlayerError" gamePlayerDecoder
        , JD.map IllegalMoveError <|
            JD.field "IllegalMoveError" ownedPlaceDecoder
        , JD.map IllegalWallLocationError <|
            JD.field "IllegalWallLocationError" ownedPlacementDecoder
        , JD.map UnknownAppearanceError <|
            JD.field "UnknownAppearanceError" JD.string
        , JD.map UnknownImageError <|
            JD.field "UnknownImageError" JD.string
        , JD.map RandomError <|
            JD.field "RandomError" JD.string
        ]


{-| This is `Json.Encode.Extra.maybe` in elm-community/json-extra.

I'm not including that whole package just for this one function.

-}
maybeNull : (a -> Value) -> Maybe a -> Value
maybeNull encoder ma =
    Maybe.map encoder ma
        |> Maybe.withDefault JE.null


messageEncoder : MessageEncoder Message
messageEncoder message =
    case message of
        PingReq { message } ->
            ( Req "ping"
            , [ ( "message", JE.string message ) ]
            )

        PongRsp { message } ->
            ( Rsp "pong"
            , [ ( "message", JE.string message ) ]
            )

        ErrorRsp { error, message } ->
            ( Rsp "error"
            , [ ( "error", errorKindEncoder error )
              , ( "message", JE.string message )
              ]
            )

        LoginWithPasswordReq { userid, passwordHash } ->
            ( Req "loginWithPassword"
            , [ ( "userid", JE.string userid )
              , ( "passwordHash", JE.string passwordHash )
              ]
            )

        LoginRsp { playerid, currentGame, allGames } ->
            ( Rsp "login"
            , [ ( "playerid", JE.string playerid )
              , ( "currentGame"
                , case currentGame of
                    Nothing ->
                        JE.null

                    Just gameid ->
                        JE.string gameid
                )
              , ( "allGames"
                , JE.list (List.map gamePlayerEncoder allGames)
                )
              ]
            )

        LogoutReq { playerid } ->
            ( Req "logout"
            , [ ( "playerid", JE.string playerid ) ]
            )

        LogoutRsp { players } ->
            ( Rsp "logout"
            , [ ( "players"
                , JE.list (List.map gamePlayerEncoder players)
                )
              ]
            )

        JoinGameReq { playerid, player } ->
            ( Req "joinGame"
            , [ ( "playerid", JE.string playerid )
              , ( "player", gamePlayerEncoder player )
              ]
            )

        NewGameReq { playerid, game } ->
            ( Req "newGame"
            , [ ( "playerid", JE.string playerid )
              , ( "game", gameEncoder game )
              ]
            )

        JoinGameRsp { player, game } ->
            ( Rsp "joinGame"
            , [ ( "player", gamePlayerEncoder player )
              , ( "game", gameEncoder game )
              ]
            )

        JoinGameNotificationRsp { player, location, direction } ->
            ( Rsp "joinGameNotification"
            , [ ( "player", gamePlayerEncoder player )
              , ( "location", locationEncoder location )
              , ( "direction", directionEncoder direction )
              ]
            )

        LeaveReq { playerid, player } ->
            ( Req "leave"
            , [ ( "playerid", JE.string playerid )
              , ( "player", gamePlayerEncoder player )
              ]
            )

        LeaveRsp { player } ->
            ( Rsp "leave"
            , [ ( "player", gamePlayerEncoder player ) ]
            )

        ExitReq { playerid, player } ->
            ( Req "exit"
            , [ ( "playerid", JE.string playerid )
              , ( "player", gamePlayerEncoder player )
              ]
            )

        ExitRsp { player } ->
            ( Rsp "exit"
            , [ ( "player", gamePlayerEncoder player ) ]
            )

        MoveReq { playerid, player, location, direction } ->
            ( Req "move"
            , [ ( "playerid", JE.string playerid )
              , ( "player", gamePlayerEncoder player )
              , ( "location", maybeNull locationEncoder location )
              , ( "direction", maybeNull directionEncoder direction )
              ]
            )

        MoveRsp { player, location, direction } ->
            ( Rsp "move"
            , [ ( "player", gamePlayerEncoder player )
              , ( "location", locationEncoder location )
              , ( "direction", directionEncoder direction )
              ]
            )

        _ ->
            ( Req "todo"
            , []
            )


messageDecoder : MessageDecoder Message
messageDecoder reqrspAndPlist =
    genericMessageDecoder reqPlist rspPlist reqrspAndPlist


reqPlist : DecoderPlist Message
reqPlist =
    [ ( "ping", pingReqDecoder )
    , ( "loginWithPassword", loginWithPasswordReqDecoder )
    , ( "logout", logoutReqDecoder )
    , ( "joinGame", joinGameReqDecoder )
    , ( "newGame", newGameReqDecoder )
    , ( "leave", leaveReqDecoder )
    , ( "exit", exitReqDecoder )
    , ( "move", moveReqDecoder )
    ]


rspPlist : DecoderPlist Message
rspPlist =
    [ ( "pong", pongRspDecoder )
    , ( "error", errorRspDecoder )
    , ( "login", loginRspDecoder )
    , ( "logout", logoutRspDecoder )
    , ( "joinGame", joinGameRspDecoder )
    , ( "joinGameNotification", joinGameNotificationRspDecoder )
    , ( "leave", leaveRspDecoder )
    , ( "exit", exitRspDecoder )
    , ( "move", moveRspDecoder )
    ]



{---- Req decoders ----}


pingReqDecoder : Decoder Message
pingReqDecoder =
    JD.map (\message -> PingReq { message = message })
        (JD.field "message" JD.string)


loginWithPasswordReqDecoder : Decoder Message
loginWithPasswordReqDecoder =
    decode
        (\userid passwordHash ->
            LoginWithPasswordReq
                { userid = userid
                , passwordHash = passwordHash
                }
        )
        |> required "userid" JD.string
        |> required "passwordHash" JD.string


logoutReqDecoder : Decoder Message
logoutReqDecoder =
    decode
        (\playerid ->
            LogoutReq { playerid = playerid }
        )
        |> required "playerid" JD.string


joinGameReqDecoder : Decoder Message
joinGameReqDecoder =
    decode
        (\playerid player ->
            JoinGameReq
                { playerid = playerid
                , player = player
                }
        )
        |> required "playerid" JD.string
        |> required "player" gamePlayerDecoder


newGameReqDecoder : Decoder Message
newGameReqDecoder =
    decode
        (\playerid game ->
            NewGameReq
                { playerid = playerid
                , game = game
                }
        )
        |> required "playerid" JD.string
        |> required "game" gameDecoder


leaveReqDecoder : Decoder Message
leaveReqDecoder =
    decode
        (\playerid player ->
            LeaveReq
                { playerid = playerid
                , player = player
                }
        )
        |> required "playerid" JD.string
        |> required "player" gamePlayerDecoder


exitReqDecoder : Decoder Message
exitReqDecoder =
    decode
        (\playerid player ->
            ExitReq
                { playerid = playerid
                , player = player
                }
        )
        |> required "playerid" JD.string
        |> required "player" gamePlayerDecoder


moveReqDecoder : Decoder Message
moveReqDecoder =
    decode
        (\playerid player location direction ->
            MoveReq
                { playerid = playerid
                , player = player
                , location = location
                , direction = direction
                }
        )
        |> required "playerid" JD.string
        |> required "player" gamePlayerDecoder
        |> required "location" (JD.nullable locationDecoder)
        |> required "direction" (JD.nullable directionDecoder)



{---- Rsp decoders ----}


pongRspDecoder : Decoder Message
pongRspDecoder =
    JD.map (\message -> PongRsp { message = message })
        (JD.field "message" JD.string)


errorRspDecoder : Decoder Message
errorRspDecoder =
    decode
        (\error message ->
            ErrorRsp
                { error = error
                , message = message
                }
        )
        |> required "error" errorKindDecoder
        |> required "message" JD.string


loginRspDecoder : Decoder Message
loginRspDecoder =
    decode
        (\playerid currentGame allGames ->
            LoginRsp
                { playerid = playerid
                , currentGame = currentGame
                , allGames = allGames
                }
        )
        |> required "playerid" JD.string
        |> required "currentGame" (JD.nullable JD.string)
        |> required "allGames" (JD.list gamePlayerDecoder)


logoutRspDecoder : Decoder Message
logoutRspDecoder =
    decode
        (\players ->
            LogoutRsp { players = players }
        )
        |> required "players" (JD.list gamePlayerDecoder)


joinGameRspDecoder : Decoder Message
joinGameRspDecoder =
    decode
        (\player game ->
            JoinGameRsp
                { player = player
                , game = game
                }
        )
        |> required "player" gamePlayerDecoder
        |> required "game" gameDecoder


joinGameNotificationRspDecoder : Decoder Message
joinGameNotificationRspDecoder =
    decode
        (\player location direction ->
            JoinGameNotificationRsp
                { player = player
                , location = location
                , direction = direction
                }
        )
        |> required "player" gamePlayerDecoder
        |> required "location" locationDecoder
        |> required "direction" directionDecoder


leaveRspDecoder : Decoder Message
leaveRspDecoder =
    decode
        (\player ->
            LeaveRsp
                { player = player
                }
        )
        |> required "player" gamePlayerDecoder


exitRspDecoder : Decoder Message
exitRspDecoder =
    decode
        (\player ->
            ExitRsp
                { player = player
                }
        )
        |> required "player" gamePlayerDecoder


moveRspDecoder : Decoder Message
moveRspDecoder =
    decode
        (\player location direction ->
            MoveRsp
                { player = player
                , location = location
                , direction = direction
                }
        )
        |> required "player" gamePlayerDecoder
        |> required "location" locationDecoder
        |> required "direction" directionDecoder
