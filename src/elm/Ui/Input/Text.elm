module Ui.Input.Text exposing
    ( init
    , setAttributes
    , setError
    , setId
    , setOnBlur
    , setOnInput
    , setPlaceholder
    , setTitle
    , setValue
    , text
    )

import Fragment.Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.Button as B exposing (ButtonMode(..))
import Ui.Group
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias TextOptions msg =
    { id : String
    , title : Maybe String
    , error : Maybe String
    , value : Maybe String
    , placeholder : String
    , onInput : Maybe (String -> msg)
    , onBlur : Maybe msg
    , attributes : List (H.Attribute msg)
    }


init : String -> TextOptions msg
init id =
    { id = id
    , title = Nothing
    , error = Nothing
    , value = Nothing
    , placeholder = ""
    , onInput = Nothing
    , onBlur = Nothing
    , attributes = []
    }


setPlaceholder : String -> TextOptions msg -> TextOptions msg
setPlaceholder placeholder opts =
    { opts | placeholder = placeholder }


setId : String -> TextOptions msg -> TextOptions msg
setId id opts =
    { opts | id = id }


setTitle : Maybe String -> TextOptions msg -> TextOptions msg
setTitle title opts =
    { opts | title = title }


setError : Maybe String -> TextOptions msg -> TextOptions msg
setError error opts =
    { opts | error = error }


setValue : Maybe String -> TextOptions msg -> TextOptions msg
setValue value opts =
    { opts | value = value }


setAttributes : List (H.Attribute msg) -> TextOptions msg -> TextOptions msg
setAttributes attributes opts =
    { opts | attributes = attributes }


setOnInput : Maybe (String -> msg) -> TextOptions msg -> TextOptions msg
setOnInput onInput opts =
    { opts | onInput = onInput }


setOnBlur : Maybe msg -> TextOptions msg -> TextOptions msg
setOnBlur onBlur opts =
    { opts | onBlur = onBlur }


text : TextOptions msg -> Html msg
text { id, title, value, error, placeholder, onInput, onBlur } =
    let
        classList =
            [ ( "ui-input-text", True ), ( "ui-input-text--error", error /= Nothing ) ]
    in
        H.label [ A.for id, A.classList classList ]
            [ Html.Extra.viewMaybe (\t -> Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ]) title
            , H.input
                [ A.type_ "text"
                , A.id id
                , A.placeholder placeholder
                , A.value <| Maybe.withDefault "" value
                , A.class "ui-input-text__input"
                , Html.Attributes.Extra.attributeMaybe (\action -> E.onInput action) onInput
                , Html.Attributes.Extra.attributeMaybe (\action -> E.onBlur action) onBlur
                ]
                []
            , Html.Extra.viewMaybe
                (\t ->
                    Text.textContainer (Just Text.DestructiveColor) <|
                        Text.Primary [ Fragment.Icon.error, H.text t ]
                )
                error
            ]
