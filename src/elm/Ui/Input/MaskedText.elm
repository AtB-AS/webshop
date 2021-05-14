module Ui.Input.MaskedText exposing
    ( State
    , init
    , initState
    , setAttributes
    , setBordered
    , setError
    , setId
    , setOnBlur
    , setOnInput
    , setPattern
    , setPlaceholder
    , setRequired
    , setTitle
    , setType
    , view
    )

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E exposing (onInput)
import Html.Extra
import MaskedInput.Text
import Ui.Button exposing (ButtonMode(..))
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias State =
    MaskedInput.Text.State


initState : MaskedInput.Text.State
initState =
    MaskedInput.Text.initialState


type alias Text msg =
    { id : String
    , pattern : String
    , title : Maybe String
    , error : Maybe String
    , type_ : String
    , required : Bool
    , placeholder : String
    , onInput : String -> msg
    , onState : State -> msg
    , onBlur : Maybe msg
    , attributes : List (H.Attribute msg)
    , bordered : Bool
    }


init : String -> (String -> msg) -> (State -> msg) -> Text msg
init id onInput onState =
    { id = id
    , pattern = ""
    , onState = onState
    , title = Nothing
    , error = Nothing
    , placeholder = ""
    , type_ = "text"
    , required = True
    , onInput = onInput
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


setType : String -> Text msg -> Text msg
setType type_ opts =
    { opts | type_ = type_ }


setPattern : String -> Text msg -> Text msg
setPattern pattern opts =
    { opts | pattern = pattern }


setRequired : Bool -> Text msg -> Text msg
setRequired required opts =
    { opts | required = required }


setAttributes : List (H.Attribute msg) -> Text msg -> Text msg
setAttributes attributes opts =
    { opts | attributes = attributes }


setOnInput : (String -> msg) -> Text msg -> Text msg
setOnInput onInput opts =
    { opts | onInput = onInput }


setOnBlur : Maybe msg -> Text msg -> Text msg
setOnBlur onBlur opts =
    { opts | onBlur = onBlur }


setBordered : Bool -> Text msg -> Text msg
setBordered bordered opts =
    { opts | bordered = bordered }


view : State -> String -> Text msg -> Html msg
view state value { id, title, type_, error, placeholder, onInput, onState, pattern, required, onBlur, attributes, bordered } =
    let
        classList =
            [ ( "ui-input-text", True )
            , ( "ui-input-text--error", error /= Nothing )
            , ( "ui-input-text--bordered", bordered )
            ]

        -- {
        --     , onInput : String -> msg
        --     , toMsg : State -> msg
        --     , hasFocus : Maybe (Bool -> msg)
        --     }
        maskedOptions : MaskedInput.Text.Options msg
        maskedOptions =
            { pattern = pattern
            , inputCharacter = '#'
            , onInput = onInput
            , toMsg = onState
            , hasFocus = Nothing
            }
    in
        H.label [ A.for id, A.classList classList ]
            [ Html.Extra.viewMaybe
                (\t ->
                    H.div [ A.class "ui-input-text__label" ] [ Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ] ]
                )
                title
            , MaskedInput.Text.input maskedOptions
                ([ A.type_ "text"
                 , A.id id
                 , A.placeholder placeholder
                 , A.type_ type_
                 , A.required required
                 , A.class "ui-input-text__input"
                 , Html.Attributes.Extra.attributeMaybe (\action -> E.onBlur action) onBlur
                 ]
                    ++ attributes
                )
                state
                value
            , Html.Extra.viewMaybe
                (\t ->
                    H.div [ A.class "ui-input-text__errorMessage" ]
                        [ Text.textContainer (Just Text.DestructiveColor) <|
                            Text.Primary [ Fragment.Icon.error, H.text t ]
                        ]
                )
                error
            ]
