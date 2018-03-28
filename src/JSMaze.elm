----------------------------------------------------------------------
--
-- JSMaze.elm
-- Simple maze game.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze exposing (..)

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
import JSMaze.Board exposing (addPlayer, canMove, simpleBoard, updatePlayer)
import JSMaze.Entities exposing (copyright, nbsp)
import JSMaze.Render exposing (render2d, render3d, renderControls)
import JSMaze.SharedTypes
    exposing
        ( Board
        , Direction(..)
        , Msg(..)
        , Operation(..)
        , Player
        , operationToDirection
        )
import JSMaze.Styles as Styles
import Keyboard exposing (KeyCode)
import List.Extra as LE
import Svg.Button as Button exposing (Button, normalRepeatTime)
import Task
import Time exposing (Time)
import Window exposing (Size)


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\x -> InitialSize x) Window.size


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { windowSize : Size
    , board : Board
    , player : Player
    , isTouchAware : Bool
    , forwardButton : Button Operation
    , backButton : Button Operation
    , subscription : Maybe ( Time, Button.Msg Msg Operation )
    }


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialPlayer : Player
initialPlayer =
    { id = "Joe Bob"
    , boardid = ""
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
    { windowSize = initialSize
    , board = addPlayer initialPlayer simpleBoard
    , player = initialPlayer
    , isTouchAware = False
    , forwardButton = initialRepeatingButton GoForward
    , backButton = initialRepeatingButton GoBack
    , subscription = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ initialSizeCmd ]


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

                        dir =
                            operationToDirection <| Button.getState button

                        mdl =
                            if isClick then
                                movePlayer dir model
                            else
                                model
                    in
                    updateButton button
                        { mdl
                            | isTouchAware =
                                if Button.isTouchAware button then
                                    True
                                else
                                    mdl.isTouchAware
                        }
                        ! [ cmd ]

        InitialSize size ->
            { model | windowSize = size }
                ! []

        Resize size ->
            { model | windowSize = size } ! []

        DownKey code ->
            processDownKey code model ! []

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


view : Model -> Html Msg
view model =
    let
        ws =
            model.windowSize

        w =
            0.9 * toFloat (min ws.width (ws.height * 2 // 3))
    in
    div [ align "center" ]
        [ Styles.style
        , h2 []
            [ text "JSMaze" ]
        , render3d w model.player model.board
        , br
        , render2d (w / 3) (Just model.player) model.board
        , space
        , renderControls (w / 3)
            model.isTouchAware
            model.forwardButton
            model.backButton
        , p []
            [ text "Use IJKL or WASD to move/rotate." ]
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
