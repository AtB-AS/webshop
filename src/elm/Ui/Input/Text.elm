module Ui.Input.Text exposing
    ( init
    , setAttributes
    , setBordered
    , setError
    , setId
    , setOnBlur
    , setOnInput
    , setPlaceholder
    , setRequired
    , setTitle
    , setType
    , setValue
    , view
    )

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.Button exposing (ButtonMode(..))
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias Text msg =
    { id : String
    , title : Maybe String
    , error : Maybe String
    , value : Maybe String
    , type_ : String
    , required : Bool
    , placeholder : String
    , onInput : Maybe (String -> msg)
    , onBlur : Maybe msg
    , attributes : List (H.Attribute msg)
    , bordered : Bool
    }


init : String -> Text msg
init id =
    { id = id
    , title = Nothing
    , error = Nothing
    , value = Nothing
    , placeholder = ""
    , type_ = "text"
    , required = True
    , onInput = Nothing
    , onBlur = Nothing
    , attributes = []
    , bordered = False
    }


setPlaceholder : String -> Text msg -> Text msg
setPlaceholder placeholder opts =
    { opts | placeholder = placeholder }


setId : String -> Text msg -> Text msg
setId id opts =
    { opts | id = id }


setTitle : Maybe String -> Text msg -> Text msg
setTitle title opts =
    { opts | title = title }


setError : Maybe String -> Text msg -> Text msg
setError error opts =
    { opts | error = error }


setValue : Maybe String -> Text msg -> Text msg
setValue value opts =
    { opts | value = value }


setType : String -> Text msg -> Text msg
setType type_ opts =
    { opts | type_ = type_ }


setRequired : Bool -> Text msg -> Text msg
setRequired required opts =
    { opts | required = required }


setAttributes : List (H.Attribute msg) -> Text msg -> Text msg
setAttributes attributes opts =
    { opts | attributes = attributes }


setOnInput : Maybe (String -> msg) -> Text msg -> Text msg
setOnInput onInput opts =
    { opts | onInput = onInput }


setOnBlur : Maybe msg -> Text msg -> Text msg
setOnBlur onBlur opts =
    { opts | onBlur = onBlur }


setBordered : Bool -> Text msg -> Text msg
setBordered bordered opts =
    { opts | bordered = bordered }


view : Text msg -> Html msg
view { id, title, value, type_, error, placeholder, onInput, required, onBlur, attributes, bordered } =
    let
        classList =
            [ ( "ui-input-text", True )
            , ( "ui-input-text--error", error /= Nothing )
            , ( "ui-input-text--bordered", bordered )
            ]
    in
        H.label [ A.for id, A.classList classList ]
            [ Html.Extra.viewMaybe
                (\t ->
                    H.div [ A.class "ui-input-text__label" ] [ Text.textContainer H.div (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ] ]
                )
                title
            , H.input
                ([ A.type_ "text"
                 , A.id id
                 , A.placeholder placeholder
                 , A.type_ type_
                 , A.required required
                 , A.value <| Maybe.withDefault "" value
                 , A.class "ui-input-text__input"
                 , Html.Attributes.Extra.attributeMaybe (\action -> E.onInput action) onInput
                 , Html.Attributes.Extra.attributeMaybe (\action -> E.onBlur action) onBlur
                 ]
                    ++ attributes
                )
                []
            , Html.Extra.viewMaybe
                (\t ->
                    H.div [ A.class "ui-input-text__errorMessage" ]
                        [ Text.textContainer H.div (Just Text.DestructiveColor) <|
                            Text.Primary [ Fragment.Icon.error, H.text t ]
                        ]
                )
                error
            ]
