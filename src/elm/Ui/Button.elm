module Ui.Button exposing
    ( Button
    , ButtonMode(..)
    , IconPosition(..)
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
    , setElement
    , setIcon
    , setIconPosition
    , setLoading
    , setOnClick
    , setText
    , setTransparent
    , setType
    , tertiary
    , tertiaryCompact
    )

import Fragment.Button
import Fragment.Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.TextContainer


type IconPosition
    = Left
    | Right


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
    , iconPosition : IconPosition
    , transparent : Bool
    , onClick : Maybe msg
    , type_ : String
    , attributes : List (H.Attribute msg)
    , element : List (Attribute msg) -> List (Html msg) -> Html msg
    , isLoading : Bool
    }


init : String -> Button msg
init text =
    { text = text
    , disabled = False
    , icon = Nothing
    , iconPosition = Right
    , transparent = False
    , onClick = Nothing
    , type_ = "button"
    , attributes = []
    , element = H.button
    , isLoading = False
    }


setDisabled : Bool -> Button msg -> Button msg
setDisabled disabled opts =
    { opts | disabled = disabled }


setLoading : Bool -> Button msg -> Button msg
setLoading isLoading opts =
    { opts | isLoading = isLoading }


setText : String -> Button msg -> Button msg
setText text opts =
    { opts | text = text }


setIcon : Maybe (Html msg) -> Button msg -> Button msg
setIcon icon opts =
    { opts | icon = icon }


setIconPosition : IconPosition -> Button msg -> Button msg
setIconPosition iconPosition opts =
    { opts | iconPosition = iconPosition }


setOnClick : Maybe msg -> Button msg -> Button msg
setOnClick onClick opts =
    { opts | onClick = onClick }


setType : String -> Button msg -> Button msg
setType type_ opts =
    { opts | type_ = type_ }


setElement : (List (Attribute msg) -> List (Html msg) -> Html msg) -> Button msg -> Button msg
setElement element opts =
    { opts | element = element }


setTransparent : Bool -> Button msg -> Button msg
setTransparent transparent opts =
    { opts | transparent = transparent }


setAttributes : List (H.Attribute msg) -> Button msg -> Button msg
setAttributes attributes opts =
    { opts | attributes = attributes }


button : ButtonMode -> ThemeColor -> Button msg -> Html msg
button mode color { text, disabled, isLoading, icon, iconPosition, transparent, onClick, type_, attributes, element } =
    let
        disabledOrLoading =
            isLoading || disabled

        classList =
            [ ( buttonModeToClass mode, True )
            , ( "ui-button", True )
            , ( "ui-button--disabled", disabledOrLoading )
            , ( "ui-button--transparent", transparent )
            , ( themeColorToClass color, mode /= Tertiary && mode /= Link )
            ]

        maybeOnClick =
            if disabledOrLoading then
                Nothing

            else
                onClick

        iconClassList =
            [ ( "ui-button__icon", True )
            , ( "ui-button__icon--left", iconPosition == Left )
            , ( "ui-button__icon--right", iconPosition == Right )
            ]

        iconEl =
            if isLoading then
                Fragment.Icon.viewMonochrome Fragment.Button.loading

            else
                Html.Extra.viewMaybe (List.singleton >> H.span [ A.classList iconClassList ]) icon
    in
        element
            ([ A.classList classList
             , Html.Attributes.Extra.attributeMaybe E.onClick maybeOnClick
             , A.attribute "aria-disabled" (boolToString disabledOrLoading)
             , A.type_ type_
             , Html.Attributes.Extra.attributeIf isLoading <| A.attribute "aria-busy" "true"
             , Html.Attributes.Extra.attributeIf isLoading <| A.attribute "aria-label" (text ++ " (Laster)")
             ]
                ++ attributes
            )
            [ Html.Extra.viewIf (iconPosition == Left) iconEl
            , Ui.TextContainer.primaryBoldInline [ H.text text ]
            , Html.Extra.viewIf (iconPosition == Right) iconEl
            ]


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
