module Ui.Input.Radio exposing
    ( RadioOptions
    , group
    , radio
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


init : String -> RadioOptions msg
init id =
    { id = id
    , name = ""
    , title = ""
    , subtitle = Nothing
    , checked = False
    , onCheck = Nothing
    }



-- setId : String -> RadioOptions msg


group : String -> List (Html msg) -> Html msg
group hiddenTitle children =
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


maybeOnCheck : Maybe (Bool -> msg) -> Attribute msg
maybeOnCheck maybeAction =
    case maybeAction of
        Just action ->
            E.onCheck action

        Nothing ->
            A.classList []
