module Ui.Button exposing
    ( ButtonMode(..)
    , ButtonOptions
    , ThemeColor(..)
    , button
    , coloredIcon
    , init
    , primary
    , primaryCompact
    , primaryDefault
    , secondary
    , secondaryCompact
    , secondaryDefault
    , setAttribues
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


type alias ButtonOptions msg =
    { text : String
    , disabled : Bool
    , icon : Maybe (Html msg)
    , onClick : Maybe msg
    , type_ : String
    , attributes : List (H.Attribute msg)
    }


init : String -> ButtonOptions msg
init text =
    { text = text
    , disabled = False
    , icon = Nothing
    , onClick = Nothing
    , type_ = "button"
    , attributes = []
    }


setDisabled : Bool -> ButtonOptions msg -> ButtonOptions msg
setDisabled disabled opts =
    { opts | disabled = disabled }


setText : String -> ButtonOptions msg -> ButtonOptions msg
setText text opts =
    { opts | text = text }


setIcon : Maybe (Html msg) -> ButtonOptions msg -> ButtonOptions msg
setIcon icon opts =
    { opts | icon = icon }


setOnClick : Maybe msg -> ButtonOptions msg -> ButtonOptions msg
setOnClick onClick opts =
    { opts | onClick = onClick }


setType : String -> ButtonOptions msg -> ButtonOptions msg
setType type_ opts =
    { opts | type_ = type_ }


setAttribues : List (H.Attribute msg) -> ButtonOptions msg -> ButtonOptions msg
setAttribues attributes opts =
    { opts | attributes = attributes }


button : ButtonMode -> ThemeColor -> ButtonOptions msg -> Html msg
button mode color { text, disabled, icon, onClick, type_, attributes } =
    let
        classList =
            [ ( buttonModeToClass mode, True )
            , ( "ui-button", True )
            , ( "ui-button--disabled", disabled )
            , ( themeColorToClass color, mode /= Tertiary )
            ]
    in
        H.button
            ([ A.classList classList
             , Html.Attributes.Extra.attributeMaybe E.onClick onClick
             , A.disabled disabled
             , A.type_ type_
             ]
                ++ attributes
            )
            [ Ui.TextContainer.primaryBold [ H.text text ], Html.Extra.viewMaybe identity icon ]


primary : ThemeColor -> ButtonOptions msg -> Html msg
primary =
    button Primary


secondary : ThemeColor -> ButtonOptions msg -> Html msg
secondary =
    button Secondary


tertiary : ButtonOptions msg -> Html msg
tertiary =
    button Tertiary Primary_2


primaryCompact : ThemeColor -> ButtonOptions msg -> Html msg
primaryCompact =
    button PrimaryCompact


secondaryCompact : ThemeColor -> ButtonOptions msg -> Html msg
secondaryCompact =
    button SecondaryCompact


tertiaryCompact : ButtonOptions msg -> Html msg
tertiaryCompact =
    button TertiaryCompact Primary_2


primaryDefault : ButtonOptions msg -> Html msg
primaryDefault =
    button Primary Primary_2


secondaryDefault : ButtonOptions msg -> Html msg
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
        >> H.div [ A.class "ui-button__coloredIcon" ]
