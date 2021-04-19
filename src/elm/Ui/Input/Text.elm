module Ui.Input.Text exposing (text)

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
    }


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
