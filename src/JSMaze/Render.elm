----------------------------------------------------------------------
--
-- Render.elm
-- Functions for rendering a JSMaze board.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.Render
    exposing
        ( render2d
        )

import Array exposing (Array)
import Debug exposing (log)
import Html exposing (Html)
import JSMaze.SharedTypes
    exposing
        ( Board
        , BoardSpec
        , Cell
        , Direction(..)
        , Location
        , Msg(..)
        , Player
        , Row
        , WallSpec
        , Walls
        )
import JSMaze.Styles exposing (SClass(..))
import JSMaze.TwoDMath exposing (Rectangle, Vector)
import Svg exposing (Svg, g, image, line, rect, svg)
import Svg.Attributes
    exposing
        ( class
        , fill
        , fontSize
        , height
        , points
        , stroke
        , transform
        , width
        , x
        , x1
        , x2
        , xlinkHref
        , y
        , y1
        , y2
        )


render2d : Float -> Maybe Player -> Board -> Html Msg
render2d w player board =
    let
        rows =
            board.rows

        cols =
            board.cols

        frows =
            toFloat rows

        fcols =
            toFloat cols

        h =
            w * frows / fcols

        delta =
            w / fcols
    in
    svg
        [ width <| toString w
        , height <| toString h
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width <| toString (w - 2)
            , height <| toString (h - 2)
            ]
            []
        , g [ class "SvgLine" ] <|
            List.indexedMap
                (render2dRow delta)
                (Array.toList board.contents)
        , case player of
            Nothing ->
                g [] []

            Just p ->
                render2dPlayer delta p
        ]


render2dRow : Float -> Int -> Row -> Svg Msg
render2dRow delta rowidx row =
    g [] <|
        List.indexedMap
            (render2dCell delta <| toFloat rowidx)
            (Array.toList row)


render2dCell : Float -> Float -> Int -> Cell -> Svg Msg
render2dCell delta rowidx colidx cell =
    let
        walls =
            cell.walls

        north =
            walls.north

        west =
            walls.west

        x1f =
            delta * toFloat colidx

        y1f =
            delta * rowidx

        x1s =
            toString x1f

        y1s =
            toString y1f

        northLine =
            if north then
                Svg.line
                    [ x1 x1s
                    , x2 (toString <| x1f + delta)
                    , y1 y1s
                    , y2 y1s
                    ]
                    []
            else
                g [] []

        westLine =
            if west then
                Svg.line
                    [ x1 x1s
                    , x2 x1s
                    , y1 y1s
                    , y2 (toString <| y1f + delta)
                    ]
                    []
            else
                g [] []
    in
    if north then
        if west then
            g [] [ northLine, westLine ]
        else
            northLine
    else
        westLine


render2dPlayer : Float -> Player -> Svg Msg
render2dPlayer delta player =
    let
        q =
            0.25 * delta

        h =
            0.5 * delta

        t =
            0.33 * delta

        ( r, c ) =
            player.location

        x =
            delta * toFloat c

        y =
            delta * toFloat r

        ( x1f, y1f, x2f, y2f, x3f, y3f ) =
            case player.direction of
                North ->
                    ( delta - t, delta - q, t, delta - q, h, q )

                South ->
                    ( t, q, delta - t, q, h, delta - q )

                East ->
                    ( q, delta - t, q, t, delta - q, h )

                West ->
                    ( delta - q, t, delta - q, delta - t, q, h )

        ( x1s, y1s, x2s, y2s, x3s, y3s ) =
            ( toString <| x + x1f
            , toString <| y + y1f
            , toString <| x + x2f
            , toString <| y + y2f
            , toString <| x + x3f
            , toString <| y + y3f
            )
    in
    Svg.polygon
        [ class "Svg2dPlayer"
        , points <| x1s ++ "," ++ y1s ++ " " ++ x2s ++ "," ++ y2s ++ " " ++ x3s ++ "," ++ y3s
        ]
        []
