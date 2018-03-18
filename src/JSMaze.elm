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
import JSMaze.Board exposing (simpleBoard)
import JSMaze.Render exposing (render2d)
import JSMaze.SharedTypes exposing (Board, Msg(..))
import JSMaze.Styles as Styles
import Keyboard exposing (KeyCode)
import List.Extra as LE
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
    }


initialSize : Size
initialSize =
    { width = 500
    , height = 500
    }


initialModel : Model
initialModel =
    { windowSize = initialSize
    , board = simpleBoard
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ initialSizeCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialSize size ->
            { model | windowSize = size }
                ! []

        Resize size ->
            { model | windowSize = size } ! []

        Nop ->
            model ! []


br : Html msg
br =
    Html.br [] []


lines : List String -> Html Msg
lines strings =
    p [] (List.concatMap (\s -> [ text s, br ]) strings)


view : Model -> Html Msg
view model =
    let
        w =
            0.5 * toFloat model.windowSize.width
    in
    div [ align "center" ]
        [ Styles.style
        , h2 []
            [ text "JSMaze" ]
        , render2d w model.board
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        ]
