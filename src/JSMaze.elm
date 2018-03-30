----------------------------------------------------------------------
--
-- JSMaze.elm
-- Simple maze game. Shared UI. Start from PortMaze or ReactorMaze.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze exposing (init, prefix, subscriptions, update, view)

import Char
import Debug exposing (log)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , blockquote
        , button
        , div
        , fieldset
        , h2
        , h3
        , h4
        , img
        , input
        , label
        , p
        , span
        , table
        , td
        , text
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( align
        , alt
        , checked
        , colspan
        , disabled
        , height
        , href
        , name
        , placeholder
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (onClick, onInput)
import JSMaze.Board
    exposing
        ( addPlayer
        , canMove
        , getCell
        , removePlayer
        , setCell
        , simpleBoard
        , updatePlayer
        )
import JSMaze.Entities exposing (copyright, nbsp)
import JSMaze.Persistence as Persistence
    exposing
        ( PersistentThing(..)
        , decodePersistentThing
        , writeBoard
        , writeModel
        , writePlayer
        )
import JSMaze.Render exposing (render2d, render3d, renderControls)
import JSMaze.SharedTypes
    exposing
        ( Board
        , Direction(..)
        , Layout(..)
        , Location
        , Model
        , Msg(..)
        , Operation(..)
        , Player
        , currentBoardId
        , currentPlayerId
        , operationToDirection
        )
import JSMaze.Styles as Styles
import Keyboard exposing (KeyCode)
import List.Extra as LE
import LocalStorage exposing (LocalStorage, setPorts)
import LocalStorage.DictPorts as DictPorts
import LocalStorage.SharedTypes exposing (Ports, Value)
import Svg.Button as Button exposing (Button, normalRepeatTime)
import Task
import Time exposing (Time)
import Window exposing (Size)


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\x -> InitialSize x) Window.size


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialPlayer : Player
initialPlayer =
    { id = currentPlayerId
    , boardid = currentBoardId
    , name = "Joe Bob"
    , location = ( 0, 0 )
    , direction = South
    }


initialButtonSize : Button.Size
initialButtonSize =
    ( 100, 100 )


initialRepeatingButton : Operation -> Button Operation
initialRepeatingButton operation =
    Button.repeatingButton normalRepeatTime initialButtonSize operation


initialModel : Model
initialModel =
    let
        board =
            addPlayer initialPlayer simpleBoard
    in
    { windowSize = initialSize
    , board = { board | id = currentBoardId }
    , player = initialPlayer
    , layout = NoLayout
    , isTouchAware = False
    , forwardButton = initialRepeatingButton GoForward
    , backButton = initialRepeatingButton GoBack
    , subscription = Nothing
    , storage = LocalStorage.make dictPorts prefix
    }


dictPorts : Ports Msg
dictPorts =
    DictPorts.make UpdatePorts prefix


{-| LocalStorage key prefix.
-}
prefix : String
prefix =
    "jsmaze"


init : Value -> Maybe (Ports Msg) -> ( Model, Cmd Msg )
init value ports =
    let
        storage =
            case ports of
                Just p ->
                    LocalStorage.make p prefix

                Nothing ->
                    initialModel.storage
    in
    { initialModel | storage = storage }
        ! [ initialSizeCmd, Persistence.initialBoard storage ]


saveModel : Model -> ( Model, Cmd Msg )
saveModel model =
    model ! [ writeModel model.storage model ]


editMaze : Model -> ( Model, Cmd Msg )
editMaze model =
    saveModel { model | layout = EditingLayout }


{-| This reverts to the default for now.

Will eventually bring up a dialog to select a saved maze.

-}
getMaze : Model -> ( Model, Cmd Msg )
getMaze model =
    let
        board =
            simpleBoard

        player =
            model.player

        ( x, y ) =
            player.location

        newPlayer =
            if x >= board.rows || y >= board.cols then
                { player
                    | location = ( 0, 0 )
                    , direction = South
                }
            else
                player
    in
    saveModel
        { model
            | board = addPlayer newPlayer board
            , layout = TopViewLayout
        }


{-| For now, this just exits editing mode.

Will eventually bring up a dialog to type a name for saving.

-}
saveMaze : Model -> ( Model, Cmd Msg )
saveMaze model =
    saveModel { model | layout = TopViewLayout }


toggleLayout : Model -> ( Model, Cmd Msg )
toggleLayout model =
    let
        layout =
            case model.layout of
                NormalLayout ->
                    TopViewLayout

                _ ->
                    NormalLayout

        mdl =
            { model | layout = layout }
    in
    mdl ! [ writeModel mdl.storage mdl ]


toggleWall : Direction -> Location -> Model -> ( Model, Cmd Msg )
toggleWall direction location model =
    case getCell location model.board of
        Nothing ->
            model ! []

        Just cell ->
            let
                walls =
                    cell.walls

                newWalls =
                    case direction of
                        West ->
                            { walls | west = not walls.west }

                        North ->
                            { walls | north = not walls.north }

                        _ ->
                            walls

                newCell =
                    { cell | walls = newWalls }

                newBoard =
                    setCell location newCell model.board

                mdl =
                    { model | board = newBoard }
            in
            mdl ! [ writeModel mdl.storage mdl ]


updateButton : Button Operation -> Model -> Model
updateButton button model =
    case Button.getState button of
        GoForward ->
            { model | forwardButton = button }

        GoBack ->
            { model | backButton = button }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePorts operation ports key value ->
            let
                mdl =
                    case ports of
                        Nothing ->
                            model

                        Just p ->
                            { model
                                | storage = setPorts p model.storage
                            }
            in
            case decodePersistentThing key value of
                Err _ ->
                    mdl ! []

                Ok thing ->
                    case thing of
                        PersistentModel savedModel ->
                            { mdl | layout = savedModel.layout }
                                ! []

                        PersistentBoard board ->
                            let
                                newBoard =
                                    addPlayer mdl.player <|
                                        { board | id = currentBoardId }
                            in
                            { mdl | board = newBoard }
                                ! []

                        PersistentPlayer player ->
                            { mdl
                                | player = player
                                , board =
                                    removePlayer mdl.player mdl.board
                                        |> addPlayer player
                            }
                                ! [ if player.id == currentPlayerId then
                                        writeBoard mdl.storage mdl.board
                                    else
                                        Cmd.none
                                  ]

        ButtonMsg msg ->
            case Button.checkSubscription msg of
                Just ( time, msg ) ->
                    { model
                        | subscription =
                            if time <= 0 then
                                Nothing
                            else
                                Just ( time, msg )
                    }
                        ! []

                Nothing ->
                    let
                        ( isClick, button, cmd ) =
                            Button.update msg

                        operation =
                            Button.getState button

                        dir =
                            operationToDirection operation

                        ( mdl, cmd2 ) =
                            if isClick then
                                case operation of
                                    ToggleLayout ->
                                        toggleLayout model

                                    ToggleWall direction location ->
                                        toggleWall direction location model

                                    EditMaze ->
                                        editMaze model

                                    GetMaze ->
                                        getMaze model

                                    SaveMaze ->
                                        saveMaze model

                                    _ ->
                                        let
                                            m =
                                                movePlayer dir model
                                        in
                                        m
                                            ! [ cmd
                                              , writePlayer model.storage m.player
                                              ]
                            else
                                model ! [ cmd ]
                    in
                    updateButton button
                        { mdl
                            | isTouchAware =
                                if Button.isTouchAware button then
                                    True
                                else
                                    mdl.isTouchAware
                        }
                        ! [ cmd2 ]

        InitialSize size ->
            { model | windowSize = size }
                ! []

        Resize size ->
            { model | windowSize = size } ! []

        DownKey code ->
            let
                mdl =
                    processDownKey code model
            in
            mdl ! [ writePlayer mdl.storage mdl.player ]

        Nop ->
            model ! []


moveDelta : Direction -> Direction -> ( Int, Int )
moveDelta dir playerDir =
    if dir == North then
        case playerDir of
            North ->
                ( -1, 0 )

            South ->
                ( 1, 0 )

            East ->
                ( 0, 1 )

            West ->
                ( 0, -1 )
    else
        case playerDir of
            North ->
                ( 1, 0 )

            South ->
                ( -1, 0 )

            East ->
                ( 0, -1 )

            West ->
                ( 0, 1 )


moveDir : Direction -> Direction -> Direction
moveDir dir playerDir =
    if dir == East then
        case playerDir of
            North ->
                East

            East ->
                South

            South ->
                West

            West ->
                North
    else
        case playerDir of
            North ->
                West

            West ->
                South

            South ->
                East

            East ->
                North


movePlayer : Direction -> Model -> Model
movePlayer dir model =
    let
        player =
            model.player

        playerDir =
            player.direction

        loc =
            player.location

        ( r, c ) =
            loc

        board =
            model.board

        ( rows, cols ) =
            ( board.rows, board.cols )

        ( newloc, newdir ) =
            if dir == North || dir == South then
                let
                    ( dr, dc ) =
                        moveDelta dir playerDir
                in
                if canMove loc ( dr, dc ) board then
                    ( ( r + dr, c + dc ), playerDir )
                else
                    ( loc, playerDir )
            else
                ( loc, moveDir dir playerDir )
    in
    if newloc == loc && newdir == playerDir then
        model
    else
        let
            newPlayer =
                { player
                    | location = newloc
                    , direction = newdir
                }

            newBoard =
                updatePlayer player newPlayer board
        in
        { model
            | board = newBoard
            , player = newPlayer
        }


upChars : List Char
upChars =
    [ 'i', 'I', 'w', 'W' ]


downChars : List Char
downChars =
    [ 'k', 'K', 's', 'S' ]


rightChars : List Char
rightChars =
    [ 'l', 'L', 'd', 'D' ]


leftChars : List Char
leftChars =
    [ 'j', 'J', 'a', 'A' ]


processDownKey : Int -> Model -> Model
processDownKey code model =
    let
        char =
            Char.fromCode code

        dir =
            if List.member char upChars then
                Just North
            else if List.member char downChars then
                Just South
            else if List.member char rightChars then
                Just East
            else if List.member char leftChars then
                Just West
            else
                Nothing
    in
    case dir of
        Nothing ->
            model

        Just d ->
            movePlayer d model


br : Html msg
br =
    Html.br [] []


lines : List String -> Html Msg
lines strings =
    p [] (List.concatMap (\s -> [ text s, br ]) strings)


sqrimg : String -> String -> Int -> Html Msg
sqrimg url name size =
    img
        [ src url
        , title name
        , alt name
        , width size
        , height size
        ]
        []


logoLink : String -> String -> String -> Int -> Html Msg
logoLink url img name size =
    a [ href url ]
        [ sqrimg ("images/" ++ img) name size ]


mailLink : String -> Html Msg
mailLink email =
    span []
        [ text "<"
        , a [ href ("mailto:" ++ email) ]
            [ text email ]
        , text ">"
        ]


space : Html Msg
space =
    text " "


renderContent : Model -> Html Msg
renderContent model =
    let
        ws =
            model.windowSize

        w =
            0.9 * toFloat (min ws.width (ws.height * 2 // 3))

        ( r1, r2 ) =
            case model.layout of
                NormalLayout ->
                    ( render3d, render2d False )

                EditingLayout ->
                    ( render2d True, render3d )

                _ ->
                    ( render2d False, render3d )
    in
    div []
        [ r1 w False model.player model.board
        , br
        , r2 (w / 3) True model.player model.board
        , space
        , renderControls (w / 3)
            model.isTouchAware
            model.layout
            model.forwardButton
            model.backButton
        ]


view : Model -> Html Msg
view model =
    if model.layout == NoLayout then
        div [] []
    else
        div [ align "center" ]
            [ Styles.style
            , h2 []
                [ text "JSMaze" ]
            , renderContent model
            , p []
                [ text "Use IJKL or WASD to move/rotate."
                , br
                , text "Click in small maze view to make it big."
                ]
            , p []
                [ text "Maze editor coming soon. " ]
            , p []
                [ logoLink "https://github.com/billstclair/elm-jsmaze"
                    "GitHub-Mark-32px.png"
                    "GitHub source code"
                    32
                , space
                , logoLink "http://elm-lang.org/"
                    "elm-logo-125x125.png"
                    "Elm inside"
                    28
                , br
                , text (copyright ++ " 2018 ")
                , a [ href "https://GibGoyGames.com/" ]
                    [ text "Gib Goy Games" ]
                , space
                , mailLink "GibGoyGames@gmail.com"
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs DownKey
        , case model.subscription of
            Nothing ->
                Sub.none

            Just ( time, msg ) ->
                Time.every time (\_ -> ButtonMsg msg)
        ]
