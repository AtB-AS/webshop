module Ui.Heading exposing
    ( component
    , componentWithEl
    , paragraph
    , paragraphWithEl
    , title
    , titleWithEl
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr


type Heading
    = Title String (Maybe String)
    | Component String (Maybe String)
    | Paragraph String (Maybe String)


headingToClass : Heading -> ( String, String, Maybe String )
headingToClass head =
    case head of
        Title text a11yLabel ->
            ( "typo-heading__title", text, a11yLabel )

        Component text a11yLabel ->
            ( "typo-heading__component", text, a11yLabel )

        Paragraph text a11yLabel ->
            ( "typo-heading__paragraph", text, a11yLabel )


headingWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> Heading -> Html msg
headingWithEl el msg =
    let
        ( class, text, a11yLabel ) =
            headingToClass msg
    in
        el [ A.class (class ++ " ui-heading"), Attr.attributeMaybe (A.attribute "aria-label") a11yLabel ] [ H.text text ]


heading : Heading -> Html msg
heading =
    headingWithEl H.h3


title : String -> Maybe String -> Html msg
title text a11yLabel =
    heading <| Title text a11yLabel


component : String -> Maybe String -> Html msg
component text a11yLabel =
    heading <| Component text a11yLabel


paragraph : String -> Maybe String -> Html msg
paragraph text a11yLabel =
    heading <| Paragraph text a11yLabel


titleWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Maybe String -> Html msg
titleWithEl el text a11yLabel =
    headingWithEl el <| Title text a11yLabel


componentWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Maybe String -> Html msg
componentWithEl el text a11yLabel =
    headingWithEl el <| Component text a11yLabel


paragraphWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Maybe String -> Html msg
paragraphWithEl el text a11yLabel =
    headingWithEl el <| Paragraph text a11yLabel
