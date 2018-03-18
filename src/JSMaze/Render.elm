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
        ( getDeltaN
        , render2d
        , render3d
        )

import Array exposing (Array)
import Debug exposing (log)
import Html exposing (Html)
import JSMaze.Board exposing (forwardDelta, getCell)
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
        , sumLocations
        )
import JSMaze.Styles exposing (SClass(..))
import JSMaze.TwoDMath exposing (Rectangle, Vector)
import Keyboard exposing (KeyCode)
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
            0.2 * delta

        h =
            0.5 * delta

        t =
            0.25 * delta

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

        p1 =
            x1s ++ "," ++ y1s

        p2 =
            x2s ++ "," ++ y2s

        p3 =
            x3s ++ "," ++ y3s
    in
    Svg.polygon
        [ class "Svg2dPlayer"
        , points <| p1 ++ " " ++ p2 ++ " " ++ p3
        ]
        []


vanishingDistance : Int
vanishingDistance =
    10


vanishingSize : Float
vanishingSize =
    0.2


wallLength : Float
wallLength =
    sqrt (2 * (((1 - vanishingSize) / 2) ^ 2))


wallConstant : Float
wallConstant =
    0.8


sumXn : Float -> Int -> Float
sumXn x n =
    if n == 0 then
        0
    else
        let
            loop =
                \i xn res ->
                    if i <= 0 then
                        res
                    else
                        loop (i - 1) (xn * x) (res + xn)
        in
        loop n x 0


xnConstant : Float
xnConstant =
    wallLength
        / sumXn wallConstant vanishingDistance


deltaN : Float -> Int -> Float
deltaN size n =
    let
        diag =
            xnConstant
                * sumXn wallConstant n
    in
    size * sqrt (diag ^ 2 / 2)


deltaNs : Array Float
deltaNs =
    List.map
        (deltaN 1)
        (List.range 1 vanishingDistance)
        |> Array.fromList


getDeltaN : Float -> Int -> Float
getDeltaN size n =
    case Array.get n deltaNs of
        Just dn ->
            size * dn

        Nothing ->
            deltaN size n


type alias RenderCell =
    { n : Int
    , lastdn : Float
    , dn : Float
    , left : Bool
    , right : Bool
    , leftright : Bool
    , rightleft : Bool
    , end : Bool
    }


computeRenderCells : Float -> Player -> Board -> List RenderCell
computeRenderCells size player board =
    let
        dir =
            player.direction

        loc =
            player.location

        ( delta, leftd, rightd, getl, getr, getf ) =
            forwardDelta dir

        ( dl, _, _, _, getlr, _ ) =
            forwardDelta leftd

        ( dr, _, _, getrl, _, _ ) =
            forwardDelta rightd

        loop : Int -> Float -> Location -> List RenderCell -> List RenderCell
        loop =
            \n lastdn loc res ->
                case getCell loc board of
                    Nothing ->
                        List.reverse res

                    Just cell ->
                        let
                            dn =
                                getDeltaN size n

                            walls =
                                cell.walls

                            left =
                                getl walls

                            right =
                                getr walls

                            locl =
                                sumLocations loc dl

                            locr =
                                sumLocations loc dr

                            leftright =
                                if left then
                                    False
                                else
                                    case getCell locl board of
                                        Nothing ->
                                            False

                                        Just lc ->
                                            getlr lc.walls

                            rightleft =
                                if right then
                                    False
                                else
                                    case getCell locr board of
                                        Nothing ->
                                            False

                                        Just rc ->
                                            getrl rc.walls

                            end =
                                getf walls

                            renderCell =
                                { n = n
                                , lastdn = lastdn
                                , dn = dn
                                , left = left
                                , right = right
                                , leftright = leftright
                                , rightleft = rightleft
                                , end = end
                                }

                            nextloc =
                                sumLocations loc delta

                            nextres =
                                renderCell :: res
                        in
                        if end then
                            List.reverse nextres
                        else
                            loop (n + 1) dn nextloc nextres
    in
    loop 1 0 loc []


ts =
    toString


render3dCell : Float -> RenderCell -> List (Svg Msg)
render3dCell w cell =
    let
        ldn =
            cell.lastdn

        dn =
            cell.dn

        end =
            cell.end

        ( lltx, llty ) =
            ( ts ldn, ts ldn )

        ( llbx, llby ) =
            ( ts ldn, ts <| w - ldn )

        ( ltx, lty ) =
            ( ts dn, ts dn )

        ( lbx, lby ) =
            ( ts dn, ts <| w - dn )

        ( rltx, rlty ) =
            ( ts <| w - ldn, ts ldn )

        ( rlbx, rlby ) =
            ( ts <| w - ldn, ts <| w - ldn )

        ( rtx, rty ) =
            ( ts <| w - dn, ts dn )

        ( rbx, rby ) =
            ( ts <| w - dn, ts <| w - dn )
    in
    [ if cell.left then
        [ Svg.line [ x1 lltx, y1 llty, x2 ltx, y2 lty ] []
        , Svg.line [ x1 llbx, y1 llby, x2 lbx, y2 lby ] []
        , if end then
            Svg.line [ x1 ltx, y1 lty, x2 lbx, y2 lby ] []
          else
            g [] []
        ]
      else
        [ Svg.line [ x1 lltx, y1 llty, x2 llbx, y2 llby ] []
        , if not end then
            Svg.line [ x1 ltx, y1 lty, x2 lbx, y2 lby ] []
          else
            g [] []
        ]
    , if cell.leftright || (end && not cell.left) then
        [ Svg.line [ x1 ltx, y1 lty, x2 lltx, y2 lty ] []
        , Svg.line [ x1 lbx, y1 lby, x2 llbx, y2 lby ] []
        ]
      else if not cell.left then
        let
            tx =
                ts (dn - (0.25 * (dn - ldn)))
        in
        [ Svg.line [ x1 ltx, y1 lty, x2 tx, y2 lty ] []
        , Svg.line [ x1 tx, y1 lty, x2 tx, y2 lby ] []
        , Svg.line [ x1 tx, y1 lby, x2 lbx, y2 lby ] []
        ]
      else
        []
    , if cell.right then
        [ Svg.line [ x1 rltx, y1 rlty, x2 rtx, y2 rty ] []
        , Svg.line [ x1 rlbx, y1 rlby, x2 rbx, y2 rby ] []
        , if end then
            Svg.line [ x1 rtx, y1 rty, x2 rbx, y2 rby ] []
          else
            g [] []
        ]
      else
        [ Svg.line [ x1 rltx, y1 rlty, x2 rlbx, y2 rlby ] []
        , if not end then
            Svg.line [ x1 rtx, y1 rty, x2 rbx, y2 rby ] []
          else
            g [] []
        ]
    , if cell.rightleft || (end && not cell.right) then
        [ Svg.line [ x1 rtx, y1 rty, x2 rltx, y2 rty ] []
        , Svg.line [ x1 rbx, y1 rby, x2 rlbx, y2 rby ] []
        ]
      else if not cell.right then
        let
            tx =
                ts (w - dn + (0.25 * (dn - ldn)))
        in
        [ Svg.line [ x1 rtx, y1 rty, x2 tx, y2 rty ] []
        , Svg.line [ x1 tx, y1 rty, x2 tx, y2 rby ] []
        , Svg.line [ x1 tx, y1 rby, x2 rbx, y2 rby ] []
        ]
      else
        []
    , if end then
        [ Svg.line [ x1 ltx, y1 lty, x2 rtx, y2 rty ] []
        , Svg.line [ x1 lbx, y1 lby, x2 rbx, y2 rby ] []
        ]
      else
        []
    ]
        |> List.concat


render3d : Float -> Player -> Board -> Html Msg
render3d w player board =
    let
        ws =
            toString w

        wm2s =
            toString (w - 2)

        renderCells =
            computeRenderCells w player board

        cells =
            List.concatMap (render3dCell w) renderCells
    in
    svg
        [ width ws
        , height ws
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width wm2s
            , height wm2s
            ]
            []
        , g [ class "SvgLine" ]
            cells
        ]
