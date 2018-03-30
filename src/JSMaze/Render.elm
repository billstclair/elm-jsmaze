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
        , renderControls
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
        , Layout(..)
        , Location
        , Msg(..)
        , Operation(..)
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
import Svg.Button as Button
    exposing
        ( Button
        , Content(..)
        , checkSubscription
        , getState
        , normalRepeatTime
        , repeatingButton
        , setSize
        )


render2d : Bool -> Float -> Bool -> Player -> Board -> Html Msg
render2d forEditing w withToggleButton player board =
    let
        rows =
            board.rows
                + (if forEditing then
                    1
                   else
                    0
                  )

        cols =
            board.cols
                + (if forEditing then
                    1
                   else
                    0
                  )

        frows =
            toFloat rows

        fcols =
            toFloat cols

        h =
            w * frows / fcols

        delta =
            w / fcols

        outerw =
            w
                - (if forEditing then
                    delta
                   else
                    0
                  )

        outerh =
            h
                - (if forEditing then
                    delta
                   else
                    0
                  )
    in
    svg
        [ width <| toString w
        , height <| toString h
        ]
        [ rect
            [ class "SvgBorder"
            , x "1"
            , y "1"
            , width <| toString (outerw - 2)
            , height <| toString (outerh - 2)
            ]
            []
        , g [ class "SvgLine" ] <|
            List.indexedMap
                (render2dRow forEditing delta)
                (Array.toList board.contents)
        , render2dPlayer delta player
        , if withToggleButton then
            renderToggleButton w h
          else
            g [] []
        ]


render2dRow : Bool -> Float -> Int -> Row -> Svg Msg
render2dRow forEditing delta rowidx row =
    g [] <|
        List.indexedMap
            (render2dCell forEditing delta rowidx)
            (Array.toList row)


render2dCell : Bool -> Float -> Int -> Int -> Cell -> Svg Msg
render2dCell forEditing delta rowidx colidx cell =
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
            delta * toFloat rowidx

        x1s =
            toString x1f

        y1s =
            toString y1f

        h =
            delta / 2

        q =
            h / 2

        x1fmq =
            toString <| x1f - q

        x1fpq =
            toString <| x1f + q

        y1fmq =
            toString <| y1f - q

        y1fpq =
            toString <| y1f + q

        hs =
            toString h

        g0 =
            g [] []

        westButton =
            if forEditing && colidx > 0 then
                g [ transform ("translate(" ++ x1fmq ++ " " ++ y1fpq ++ ")") ]
                    [ renderOverlayButton (ToggleWall West ( rowidx, colidx )) h h ]
            else
                g0

        northButton =
            if forEditing && rowidx > 0 then
                g [ transform ("translate(" ++ x1fpq ++ " " ++ y1fmq ++ ")") ]
                    [ renderOverlayButton (ToggleWall North ( rowidx, colidx )) h h ]
            else
                g0

        northLine =
            if north && rowidx > 0 then
                Svg.line
                    [ x1 x1s
                    , x2 (toString <| x1f + delta)
                    , y1 y1s
                    , y2 y1s
                    ]
                    []
            else
                g0

        westLine =
            if west && colidx > 0 then
                Svg.line
                    [ x1 x1s
                    , x2 x1s
                    , y1 y1s
                    , y2 (toString <| y1f + delta)
                    ]
                    []
            else
                g0
    in
    if north || forEditing then
        if west || forEditing then
            g [] [ northLine, westLine, northButton, westButton ]
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


renderToggleButton =
    renderOverlayButton ToggleLayout


renderOverlayButton : Operation -> Float -> Float -> Svg Msg
renderOverlayButton operation width height =
    let
        button =
            Button.simpleButton ( width, height ) operation
    in
    Button.renderOverlay ButtonMsg button


vanishingDistance : Int
vanishingDistance =
    12


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


render3d : Float -> Bool -> Player -> Board -> Html Msg
render3d w withToggleButton player board =
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
        , if withToggleButton then
            renderToggleButton w w
          else
            g [] []
        ]


simpleButton : Button.Size -> Operation -> Bool -> Button Operation
simpleButton size operation isTouchAware =
    let
        button =
            Button.simpleButton size operation
    in
    Button.setTouchAware isTouchAware button


repeatingButton : Button.Size -> Operation -> Button Operation
repeatingButton size operation =
    Button.repeatingButton normalRepeatTime size operation


renderControls : Float -> Bool -> Layout -> Button Operation -> Button Operation -> Html Msg
renderControls w isTouchAware layout forwardButton reverseButton =
    let
        ws =
            toString w

        bw =
            w / 2

        bw3s =
            toString (bw * 3)

        leftRightY =
            if layout == NormalLayout then
                bw / 2 + 2
            else
                2

        size : Button.Size
        size =
            ( bw, bw )
    in
    svg
        [ width bw3s
        , height ws
        ]
        [ Button.render
            ( 2, leftRightY )
            (TextContent "<")
            ButtonMsg
            (simpleButton size TurnLeft isTouchAware)
        , Button.render
            ( 2 * bw - 2, leftRightY )
            (TextContent ">")
            ButtonMsg
            (simpleButton size TurnRight isTouchAware)
        , Button.render
            ( bw, 2 )
            (TextContent "^")
            ButtonMsg
            (setSize size forwardButton)
        , Button.render
            ( bw, bw )
            (TextContent "v")
            ButtonMsg
            (setSize size reverseButton)
        , case layout of
            TopViewLayout ->
                g []
                    [ Button.render
                        ( 2, bw )
                        (TextContent "Edit")
                        ButtonMsg
                        (simpleButton size EditMaze isTouchAware)
                    , Button.render
                        ( 2 * bw - 2, bw )
                        (TextContent "Get")
                        ButtonMsg
                        (simpleButton size GetMaze isTouchAware)
                    ]

            EditingLayout ->
                g []
                    [ Button.render
                        ( 2, bw )
                        (TextContent "Save")
                        ButtonMsg
                        (simpleButton size SaveMaze isTouchAware)
                    , Button.render
                        ( 2 * bw - 2, bw )
                        (TextContent "Init")
                        ButtonMsg
                        (simpleButton size GetMaze isTouchAware)
                    ]

            _ ->
                g [] []
        ]
