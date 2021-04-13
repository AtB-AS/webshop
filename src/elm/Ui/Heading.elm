module Ui.Heading exposing
    ( component
    , paragraph
    , title
    )

import Html as H exposing (Html)
import Html.Attributes as A


type Heading
    = Title String
    | Component String
    | Paragraph String


headingToClass : Heading -> ( String, String )
headingToClass head =
    case head of
        Title text ->
            ( "typo-heading__title", text )

        Component text ->
            ( "typo-heading__component", text )

        Paragraph text ->
            ( "typo-heading__paragraph", text )


heading : Heading -> Html msg
heading msg =
    let
        ( class, text ) =
            headingToClass msg
    in
        H.h3 [ A.class (class ++ " ui-heading") ] [ H.text text ]


title : String -> Html msg
title =
    Title >> heading


component : String -> Html msg
component =
    Component >> heading


paragraph : String -> Html msg
paragraph =
    Paragraph >> heading
