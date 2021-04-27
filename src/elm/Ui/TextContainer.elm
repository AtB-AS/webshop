module Ui.TextContainer exposing
    ( TextColor(..)
    , TextContainer(..)
    , primary
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


type TextColor
    = PrimaryColor
    | SecondaryColor
    | DisabledColor
    | DestructiveColor


type TextContainer msg
    = Primary (List (Html msg))
    | PrimaryBold (List (Html msg))
    | PrimaryUnderline (List (Html msg))
    | PrimaryJumbo (List (Html msg))
    | Secondary (List (Html msg))
    | SecondaryBold (List (Html msg))
    | Tertiary (List (Html msg))
    | TertiaryStrike (List (Html msg))


toClassChildrenPair : TextContainer msg -> Maybe TextColor -> ( String, List (Html msg) )
toClassChildrenPair head color =
    let
        textColorClass =
            case color of
                Just PrimaryColor ->
                    "ui-textContainer--primary"

                Just SecondaryColor ->
                    "ui-textContainer--secondary"

                Just DisabledColor ->
                    "ui-textContainer--disabled"

                Just DestructiveColor ->
                    "ui-textContainer--destructive"

                Nothing ->
                    ""
    in
        case head of
            Primary text ->
                ( "typo-body__primary ui-textContainer " ++ textColorClass, text )

            PrimaryBold text ->
                ( "typo-body__primary--bold ui-textContainer " ++ textColorClass, text )

            PrimaryUnderline text ->
                ( "typo-body__primary--underline ui-textContainer " ++ textColorClass, text )

            PrimaryJumbo text ->
                ( "typo-body__primary--jumbo ui-textContainer " ++ textColorClass, text )

            Secondary text ->
                ( "typo-body__secondary ui-textContainer " ++ textColorClass, text )

            SecondaryBold text ->
                ( "typo-body__secondary--bold ui-textContainer " ++ textColorClass, text )

            Tertiary text ->
                ( "typo-body__tertiary ui-textContainer " ++ textColorClass, text )

            TertiaryStrike text ->
                ( "typo-body__tertiary--strike ui-textContainer " ++ textColorClass, text )


textContainer : Maybe TextColor -> TextContainer msg -> Html msg
textContainer color msg =
    let
        ( class, children ) =
            toClassChildrenPair msg color
    in
        H.span [ A.class class ] children


primary : List (Html msg) -> Html msg
primary =
    Primary >> textContainer Nothing


primaryBold : List (Html msg) -> Html msg
primaryBold =
    PrimaryBold >> textContainer Nothing


primaryUnderline : List (Html msg) -> Html msg
primaryUnderline =
    PrimaryUnderline >> textContainer Nothing


primaryJumbo : List (Html msg) -> Html msg
primaryJumbo =
    PrimaryJumbo >> textContainer Nothing


secondary : List (Html msg) -> Html msg
secondary =
    Secondary >> textContainer Nothing


secondaryBold : List (Html msg) -> Html msg
secondaryBold =
    SecondaryBold >> textContainer Nothing


tertiary : List (Html msg) -> Html msg
tertiary =
    Tertiary >> textContainer Nothing


tertiaryStrike : List (Html msg) -> Html msg
tertiaryStrike =
    TertiaryStrike >> textContainer Nothing
