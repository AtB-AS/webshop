module Ui.Heading exposing
    ( component
    , componentWithEl
    , page
    , paragraph
    , paragraphWithEl
    , title
    , titleWithEl
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
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


headingWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> Heading -> Html msg
headingWithEl el msg =
    let
        ( class, text ) =
            headingToClass msg
    in
        el [ A.class (class ++ " ui-heading") ] [ H.text text ]


heading : Heading -> Html msg
heading =
    headingWithEl H.h3


title : String -> Html msg
title =
    Title >> heading


component : String -> Html msg
component =
    Component >> heading


paragraph : String -> Html msg
paragraph =
    Paragraph >> heading


titleWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
titleWithEl el =
    Title >> headingWithEl el


componentWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
componentWithEl el =
    Component >> headingWithEl el


paragraphWithEl : (List (Attribute msg) -> List (Html msg) -> Html msg) -> String -> Html msg
paragraphWithEl el =
    Paragraph >> headingWithEl el


page : String -> String -> Html msg
page pageTitle backTitle =
    H.div [ A.class "ui-heading-page" ]
        [ H.a [ A.href "#/", A.class "ui-heading-page__back" ] [ Icon.leftArrow, H.text backTitle ]
        , componentWithEl H.h2 pageTitle
        ]
