module Ui.Input exposing (..)

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E


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
            :: children
        )


radio : RadioOptions msg -> Html msg
radio { id, name, title, onCheck, checked } =
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
            [ H.div [ A.class "ui-input-radio__title" ] [ H.text title ]
            , H.div [ A.class "ui-input-radio__box" ] []
            ]
        ]


maybeOnCheck : Maybe (Bool -> msg) -> Attribute msg
maybeOnCheck maybeAction =
    case maybeAction of
        Just action ->
            E.onCheck action

        Nothing ->
            A.classList []
