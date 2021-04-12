module Ui.TextContainer exposing
    ( primary
    , primaryBold
    , primaryJumbo
    , primaryUnderline
    , secondary
    , secondaryBold
    , tertiary
    , tertiaryStrike
    , textContainer
    )

import Html as H exposing (Html)
import Html.Attributes as A


type TextContainer msg
    = Primary (List (Html msg))
    | PrimaryBold (List (Html msg))
    | PrimaryUnderline (List (Html msg))
    | PrimaryJumbo (List (Html msg))
    | Secondary (List (Html msg))
    | SecondaryBold (List (Html msg))
    | Tertiary (List (Html msg))
    | TertiaryStrike (List (Html msg))


toClassChildrenPair : TextContainer msg -> ( String, List (Html msg) )
toClassChildrenPair head =
    case head of
        Primary text ->
            ( "typo-body__primary", text )

        PrimaryBold text ->
            ( "typo-body__primary--bold", text )

        PrimaryUnderline text ->
            ( "typo-body__primary--underline", text )

        PrimaryJumbo text ->
            ( "typo-body__primary--jumbo", text )

        Secondary text ->
            ( "typo-body__secondary", text )

        SecondaryBold text ->
            ( "typo-body__secondary--bold", text )

        Tertiary text ->
            ( "typo-body__tertiary", text )

        TertiaryStrike text ->
            ( "typo-body__tertiary--strike", text )


textContainer : TextContainer msg -> Html msg
textContainer msg =
    let
        ( class, children ) =
            toClassChildrenPair msg
    in
        H.div [ A.class class ] children


primary : List (Html msg) -> Html msg
primary =
    Primary >> textContainer


primaryBold : List (Html msg) -> Html msg
primaryBold =
    PrimaryBold >> textContainer


primaryUnderline : List (Html msg) -> Html msg
primaryUnderline =
    PrimaryUnderline >> textContainer


primaryJumbo : List (Html msg) -> Html msg
primaryJumbo =
    PrimaryJumbo >> textContainer


secondary : List (Html msg) -> Html msg
secondary =
    Secondary >> textContainer


secondaryBold : List (Html msg) -> Html msg
secondaryBold =
    SecondaryBold >> textContainer


tertiary : List (Html msg) -> Html msg
tertiary =
    Tertiary >> textContainer


tertiaryStrike : List (Html msg) -> Html msg
tertiaryStrike =
    TertiaryStrike >> textContainer
