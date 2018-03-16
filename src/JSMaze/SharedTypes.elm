----------------------------------------------------------------------
--
-- SharedTypes.elm
-- Types used everywhere.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.SharedTypes
    exposing
        ( Msg(..)
        )

import Window exposing (Size)


type Msg
    = InitialSize Size
    | Resize Size
    | Nop
