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


render2d : Float -> Board -> Html Msg
render2d w board =
    svg
        [ width <| toString w
        , height <| toString w
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width <| toString (w - 2)
            , height <| toString (w - 2)
            ]
            []
        ]
