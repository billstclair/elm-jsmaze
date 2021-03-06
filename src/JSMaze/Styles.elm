----------------------------------------------------------------------
--
-- Styles.elm
-- The CSS Stylesheet for the JSMaze game.
-- Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module JSMaze.Styles exposing (SClass(..), class, classes, style)

import Css exposing (Sel(..))
import Html.Attributes


type SClass
    = Error
      -- SVG Classes
    | SvgBorder
    | SvgLine
    | Svg2dPlayer
    | SvgLabel
    | SvgLabelText
    | SvgCell
    | SvgCellColor
    | SvgCellText
    | SvgObjectColor
    | SvgEditorHighlight


imports : List String
imports =
    []


rule : a -> b -> { selectors : a, descriptor : b }
rule selectors descriptor =
    { selectors = selectors
    , descriptor = descriptor
    }


greenColor : String
greenColor =
    "#E0FFE0"


rules =
    [ rule
        [ Class Error ]
        [ ( "background-color", "red" )
        , ( "color", "white" )
        ]

    -- SVG classes
    , rule
        [ Class SvgLabel ]
        [ ( "padding", "2px" )
        , ( "fill", "#ececec" )
        , ( "stroke", "white" )
        , ( "stroke-width", "1" )
        ]
    , rule
        [ Class SvgLabelText ]
        [ ( "fill", "black" )
        , ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgCell ]
        [ ( "fill-opacity", "1.0" )
        , ( "stroke", "black" )
        , ( "stroke-width", "1px" )
        ]
    , rule
        [ Class SvgBorder ]
        [ ( "fill-opacity", "1.0" )
        , ( "stroke", "black" )
        , ( "fill", "white" )
        , ( "stroke-width", "2px" )
        ]
    , rule
        [ Class SvgLine ]
        [ ( "stroke", "black" )
        , ( "stroke-width", "2px" )
        ]
    , rule
        [ Class Svg2dPlayer ]
        [ ( "stroke", "darkgray" )
        , ( "stroke-width", "1px" )
        , ( "fill", "darkgray" )
        ]
    , rule
        [ Class SvgCellColor ]
        [ ( "fill", "white" )
        ]
    , rule
        [ Class SvgObjectColor ]
        [ ( "fill", "gray" )
        ]
    , rule
        [ Class SvgCellText ]
        [ ( "font-weight", "bold" )
        ]
    , rule
        [ Class SvgEditorHighlight ]
        [ ( "stroke", "lightblue" )
        , ( "fill", "lightblue" )
        , ( "stroke-width", "1px" )
        , ( "stroke-opacity", "0.1" )
        , ( "fill-opacity", "0.1" )
        ]
    ]


stylesheet =
    Css.stylesheet imports rules



-- This is for inclusion at the beginning of the Board div


style =
    Css.style [ Html.Attributes.scoped True ] stylesheet



-- For use in the attributes of Html elements


class =
    stylesheet.class


classes =
    stylesheet.classes
