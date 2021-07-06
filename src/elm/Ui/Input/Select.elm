module Ui.Input.Select exposing
    ( init
    , setAttributes
    , setBordered
    , setError
    , setId
    , setOnInput
    , setRequired
    , setTitle
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


type alias Select msg =
    { id : String
    , title : Maybe String
    , error : Maybe String
    , required : Bool
    , onInput : Maybe (String -> msg)
    , attributes : List (H.Attribute msg)
    , bordered : Bool
    }


init : String -> Select msg
init id =
    { id = id
    , title = Nothing
    , error = Nothing
    , required = True
    , onInput = Nothing
    , attributes = []
    , bordered = False
    }


setId : String -> Select msg -> Select msg
setId id opts =
    { opts | id = id }


setTitle : Maybe String -> Select msg -> Select msg
setTitle title opts =
    { opts | title = title }


setError : Maybe String -> Select msg -> Select msg
setError error opts =
    { opts | error = error }


setRequired : Bool -> Select msg -> Select msg
setRequired required opts =
    { opts | required = required }


setAttributes : List (H.Attribute msg) -> Select msg -> Select msg
setAttributes attributes opts =
    { opts | attributes = attributes }


setOnInput : Maybe (String -> msg) -> Select msg -> Select msg
setOnInput onInput opts =
    { opts | onInput = onInput }


setBordered : Bool -> Select msg -> Select msg
setBordered bordered opts =
    { opts | bordered = bordered }


view : List (Html msg) -> Select msg -> Html msg
view children { id, title, error, onInput, required, attributes, bordered } =
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
                    H.span [ A.class "ui-input-text__label" ] [ Text.textContainer H.span (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ] ]
                )
                title
            , H.select
                ([ A.id id
                 , A.required required
                 , A.class "ui-input-text__input"
                 , Html.Attributes.Extra.attributeMaybe (\action -> E.onInput action) onInput
                 ]
                    ++ attributes
                )
                children
            , Html.Extra.viewMaybe
                (\t ->
                    Text.textContainer H.span (Just Text.DestructiveColor) <|
                        Text.Primary [ H.span [ A.class "ui-input-text__errorMessage", A.attribute "role" "alert" ] [ Fragment.Icon.error, H.text t ] ]
                )
                error
            ]
