----------------------------------------------------------------------
--
-- TwoDMath.elm
-- Types used everywhere.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.TwoDMath
    exposing
        ( Rectangle
        , Vector
        , combineVectors
        , distanceToRectangle
        , isVectorInRectangle
        , makeRectangle
        , makeSize
        , makeVector
        , positionToVector
        , rectangleCenter
        , rectangleCoordinates
        , rectangleFromVectors
        , sizeToVector
        , vectorCoordinates
        , vectorDifference
        , vectorDistance
        , vectorSum
        , zeroRectangle
        , zeroVector
        )

import Mouse exposing (Position)
import Window exposing (Size)


type alias Vector =
    { x : Float
    , y : Float
    }


makeVector : Float -> Float -> Vector
makeVector x y =
    { x = x, y = y }


vectorCoordinates : Vector -> ( Float, Float )
vectorCoordinates vector =
    ( vector.x, vector.y )


sizeToVector : Size -> Vector
sizeToVector size =
    makeVector (toFloat size.width) (toFloat size.height)


positionToVector : Position -> Vector
positionToVector pos =
    makeVector (toFloat pos.x) (toFloat pos.y)


zeroVector : Vector
zeroVector =
    makeVector 0 0


combineVectors : (Float -> Float -> Float) -> Vector -> Vector -> Vector
combineVectors f v1 v2 =
    makeVector (f v1.x v2.x) (f v1.y v2.y)


vectorSum =
    combineVectors (+)


vectorDifference =
    combineVectors (-)


vectorDistance : Vector -> Vector -> Float
vectorDistance v1 v2 =
    let
        diff =
            vectorDifference v1 v2
    in
    sqrt (diff.x ^ 2 + diff.y ^ 2)


type alias Rectangle =
    { pos : Vector --top-left corner
    , size : Vector
    }


{-| Result is (left, top, right bottom)
-}
rectangleCoordinates : Rectangle -> ( Float, Float, Float, Float )
rectangleCoordinates rect =
    let
        pos =
            rect.pos

        size =
            rect.size

        left =
            pos.x

        top =
            pos.y

        right =
            left + size.x

        bottom =
            top + size.y
    in
    ( left, top, right, bottom )


makeRectangle : Float -> Float -> Float -> Float -> Rectangle
makeRectangle left top width height =
    rectangleFromVectors
        (makeVector left top)
        (makeVector width height)


rectangleFromVectors : Vector -> Vector -> Rectangle
rectangleFromVectors pos size =
    { pos = pos
    , size = size
    }


zeroRectangle : Rectangle
zeroRectangle =
    rectangleFromVectors zeroVector zeroVector


rectangleCenter : Rectangle -> Vector
rectangleCenter rect =
    let
        ( left, top, right, bottom ) =
            rectangleCoordinates rect
    in
    makeVector ((left + right) / 2) ((top + bottom) / 2)


distanceToRectangle : Vector -> Rectangle -> Float
distanceToRectangle vect rect =
    let
        center =
            rectangleCenter rect
    in
    vectorDistance vect center


isVectorInRectangle : Vector -> Rectangle -> Bool
isVectorInRectangle vect rect =
    let
        ( left, top, right, bottom ) =
            rectangleCoordinates rect

        ( x, y ) =
            vectorCoordinates vect
    in
    x >= left && x <= right && y >= top && y <= bottom


makeSize : Int -> Int -> Size
makeSize w h =
    { width = w, height = h }
