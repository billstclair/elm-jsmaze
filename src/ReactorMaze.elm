----------------------------------------------------------------------
--
-- ReactorMaze.elm
-- Top-level for elm-reactor based application.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module ReactorMaze exposing (..)

import Html
import JSMaze exposing (init, prefix, update, view)
import JSMaze.Types exposing (Model, Msg(..))
import Json.Encode as JE
import Task


main =
    Html.program
        { init = init JE.null Nothing
        , view = view
        , update = update
        , subscriptions = JSMaze.subscriptions
        }
