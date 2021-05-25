module Ui.Button exposing
    ( Button
    , ButtonMode(..)
    , ThemeColor(..)
    , button
    , coloredIcon
    , init
    , link
    , primary
    , primaryCompact
    , primaryDefault
    , secondary
    , secondaryCompact
    , secondaryDefault
    , setAttributes
    , setDisabled
    , setIcon
    , setOnClick
    , setText
    , setType
    , tertiary
    , tertiaryCompact
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.TextContainer


type ButtonMode
    = Primary
    | Secondary
    | Tertiary
    | Link
    | PrimaryCompact
    | SecondaryCompact
    | TertiaryCompact


type ThemeColor
    = Primary_1
    | Primary_2
    | Primary_3
    | Primary_destructive
    | Secondary_1
    | Secondary_2
    | Secondary_3
    | Secondary_4


type alias Button msg =
    { text : String
    , disabled : Bool
    , icon : Maybe (Html msg)
    , onClick : Maybe msg
    , type_ : String
    , attributes : List (H.Attribute msg)
    }


init : String -> Button msg
init text =
    { text = text
    , disabled = False
    , icon = Nothing
    , onClick = Nothing
    , type_ = "button"
    , attributes = []
    }


setDisabled : Bool -> Button msg -> Button msg
setDisabled disabled opts =
    { opts | disabled = disabled }


setText : String -> Button msg -> Button msg
setText text opts =
    { opts | text = text }


setIcon : Maybe (Html msg) -> Button msg -> Button msg
setIcon icon opts =
    { opts | icon = icon }


setOnClick : Maybe msg -> Button msg -> Button msg
setOnClick onClick opts =
    { opts | onClick = onClick }


setType : String -> Button msg -> Button msg
setType type_ opts =
    { opts | type_ = type_ }


setAttributes : List (H.Attribute msg) -> Button msg -> Button msg
setAttributes attributes opts =
    { opts | attributes = attributes }


button : ButtonMode -> ThemeColor -> Button msg -> Html msg
button mode color { text, disabled, icon, onClick, type_, attributes } =
    let
        classList =
            [ ( buttonModeToClass mode, True )
            , ( "ui-button", True )
            , ( "ui-button--disabled", disabled )
            , ( themeColorToClass color, mode /= Tertiary && mode /= Link )
            ]

        maybeOnClick =
            if disabled then
                Nothing

            else
                onClick
    in
        H.button
            ([ A.classList classList
             , Html.Attributes.Extra.attributeMaybe E.onClick maybeOnClick
             , A.attribute "aria-disabled" (boolToString disabled)
             , A.type_ type_
             ]
                ++ attributes
            )
            [ Ui.TextContainer.primaryBoldInline [ H.text text ], Html.Extra.viewMaybe (List.singleton >> H.span [ A.class "ui-button__icon" ]) icon ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


primary : ThemeColor -> Button msg -> Html msg
primary =
    button Primary


secondary : ThemeColor -> Button msg -> Html msg
secondary =
    button Secondary


tertiary : Button msg -> Html msg
tertiary =
    button Tertiary Primary_2


link : Button msg -> Html msg
link =
    button Link Primary_2


primaryCompact : ThemeColor -> Button msg -> Html msg
primaryCompact =
    button PrimaryCompact


secondaryCompact : ThemeColor -> Button msg -> Html msg
secondaryCompact =
    button SecondaryCompact


tertiaryCompact : Button msg -> Html msg
tertiaryCompact =
    button TertiaryCompact Primary_2


primaryDefault : Button msg -> Html msg
primaryDefault =
    button Primary Primary_2


secondaryDefault : Button msg -> Html msg
secondaryDefault =
    button Secondary Primary_2


buttonModeToClass : ButtonMode -> String
buttonModeToClass mode =
    case mode of
        Primary ->
            "ui-button--primary"

        Secondary ->
            "ui-button--secondary"

        Tertiary ->
            "ui-button--tertiary"

        Link ->
            "ui-button--link"

        PrimaryCompact ->
            "ui-button--primary ui-button--compact"

        SecondaryCompact ->
            "ui-button--secondary ui-button--compact"

        TertiaryCompact ->
            "ui-button--tertiary ui-button--compact"


themeColorToClass : ThemeColor -> String
themeColorToClass color =
    case color of
        Primary_1 ->
            "colors-primary_1"

        Primary_2 ->
            "colors-primary_2"

        Primary_3 ->
            "colors-primary_3"

        Primary_destructive ->
            "colors-primary_destructive"

        Secondary_1 ->
            "colors-secondary_1"

        Secondary_2 ->
            "colors-secondary_2"

        Secondary_3 ->
            "colors-secondary_3"

        Secondary_4 ->
            "colors-secondary_4"


coloredIcon : Html msg -> Html msg
coloredIcon =
    List.singleton
        >> H.span [ A.class "ui-button__coloredIcon" ]
