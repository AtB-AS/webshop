module Ui.Input exposing
    ( RadioOptions
    , editSection
    , radio
    , radioGroup
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


type alias RadioOptions msg =
    { id : String
    , name : String
    , title : String
    , subtitle : Maybe String
    , checked : Bool
    , onCheck : Maybe (Bool -> msg)
    }


radioGroup : String -> List (Html msg) -> Html msg
radioGroup hiddenTitle children =
    H.fieldset [ A.class "ui-input-radioGroup" ]
        (H.legend
            [ A.class "ui-input-radioGroup__hiddenLegend" ]
            [ H.text hiddenTitle ]
            :: List.map Ui.Group.groupItem children
        )


radio : RadioOptions msg -> Html msg
radio { id, name, title, onCheck, checked, subtitle } =
    H.div []
        [ H.input
            [ A.type_ "radio"
            , A.id id
            , A.name name
            , A.class "ui-input-radio__input"
            , maybeOnCheck onCheck
            , A.checked checked
            ]
            []
        , H.label [ A.for id, A.class "ui-input-radio" ]
            [ H.div [ A.class "ui-input-radio__title" ]
                [ H.text title
                , Html.Extra.viewMaybe (\t -> Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ]) subtitle
                ]
            , H.div [ A.class "ui-input-radio__box" ] []
            ]
        ]


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


maybeOnCheck : Maybe (Bool -> msg) -> Attribute msg
maybeOnCheck maybeAction =
    case maybeAction of
        Just action ->
            E.onCheck action

        Nothing ->
            A.classList []


type alias EditSectionOptions msg =
    { accessibilityName : String
    , editText : String
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , onCancel : Maybe msg
    , inEditMode : Bool
    }


editSection : EditSectionOptions msg -> (Bool -> List (Html msg)) -> Html msg
editSection { accessibilityName, editText, onEdit, onSave, onCancel, inEditMode } children =
    H.form [ A.method "post", Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave ]
        [ if not inEditMode then
            H.div []
                (children False
                    ++ [ B.init editText
                            |> B.setIcon (Just Fragment.Icon.edit)
                            |> B.setOnClick onEdit
                            |> B.tertiaryCompact
                       ]
                )

          else
            H.fieldset []
                (H.legend
                    []
                    [ H.text accessibilityName ]
                    :: children True
                    ++ [ H.div []
                            [ B.init "Avbryt"
                                |> B.setIcon (Just Fragment.Icon.cross)
                                |> B.setOnClick onCancel
                                |> B.tertiaryCompact
                            , B.init "Lagre"
                                |> B.setIcon (Just Fragment.Icon.checkmark)
                                |> B.setOnClick onSave
                                |> B.setType "submit"
                                |> B.primaryCompact B.Secondary_2
                            ]
                       ]
                )
        ]
