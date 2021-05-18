module Ui.TextContainer exposing
    ( TextColor(..)
    , TextContainer(..)
    , primary
    , primaryBold
    , primaryBoldInline
    , primaryInline
    , primaryJumbo
    , primaryJumboInline
    , primaryUnderline
    , primaryUnderlineInline
    , secondary
    , secondaryBold
    , secondaryBoldInline
    , secondaryInline
    , tertiary
    , tertiaryInline
    , tertiaryStrike
    , tertiaryStrikeInline
    , textContainer
    )

import Html as H exposing (Attribute, Html)
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


textContainer : (List (Attribute msg) -> List (Html msg) -> Html msg) -> Maybe TextColor -> TextContainer msg -> Html msg
textContainer el color msg =
    let
        ( class, children ) =
            toClassChildrenPair msg color
    in
        el [ A.class class ] children


primary : List (Html msg) -> Html msg
primary =
    Primary >> textContainer H.div Nothing


primaryBold : List (Html msg) -> Html msg
primaryBold =
    PrimaryBold >> textContainer H.div Nothing


primaryUnderline : List (Html msg) -> Html msg
primaryUnderline =
    PrimaryUnderline >> textContainer H.div Nothing


primaryJumbo : List (Html msg) -> Html msg
primaryJumbo =
    PrimaryJumbo >> textContainer H.div Nothing


secondary : List (Html msg) -> Html msg
secondary =
    Secondary >> textContainer H.div Nothing


secondaryBold : List (Html msg) -> Html msg
secondaryBold =
    SecondaryBold >> textContainer H.div Nothing


tertiary : List (Html msg) -> Html msg
tertiary =
    Tertiary >> textContainer H.div Nothing


tertiaryStrike : List (Html msg) -> Html msg
tertiaryStrike =
    TertiaryStrike >> textContainer H.div Nothing


primaryInline : List (Html msg) -> Html msg
primaryInline =
    Primary >> textContainer H.span Nothing


primaryBoldInline : List (Html msg) -> Html msg
primaryBoldInline =
    PrimaryBold >> textContainer H.span Nothing


primaryUnderlineInline : List (Html msg) -> Html msg
primaryUnderlineInline =
    PrimaryUnderline >> textContainer H.span Nothing


primaryJumboInline : List (Html msg) -> Html msg
primaryJumboInline =
    PrimaryJumbo >> textContainer H.span Nothing


secondaryInline : List (Html msg) -> Html msg
secondaryInline =
    Secondary >> textContainer H.span Nothing


secondaryBoldInline : List (Html msg) -> Html msg
secondaryBoldInline =
    SecondaryBold >> textContainer H.span Nothing


tertiaryInline : List (Html msg) -> Html msg
tertiaryInline =
    Tertiary >> textContainer H.span Nothing


tertiaryStrikeInline : List (Html msg) -> Html msg
tertiaryStrikeInline =
    TertiaryStrike >> textContainer H.span Nothing
