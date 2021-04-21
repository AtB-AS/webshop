module Ui.Input.Radio exposing
    ( RadioOptions
    , group
    , init
    , radio
    , setAttributes
    , setChecked
    , setId
    , setName
    , setOnCheck
    , setSubtitle
    , setTitle
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Ui.Button exposing (ButtonMode(..))
import Ui.Group
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias RadioOptions msg =
    { id : String
    , name : String
    , title : String
    , subtitle : Maybe String
    , checked : Bool
    , onCheck : Maybe (Bool -> msg)
    , attributes : List (H.Attribute msg)
    }


init : String -> RadioOptions msg
init id =
    { id = id
    , name = ""
    , title = ""
    , subtitle = Nothing
    , checked = False
    , onCheck = Nothing
    , attributes = []
    }


setId : String -> RadioOptions msg -> RadioOptions msg
setId id opts =
    { opts | id = id }


setName : String -> RadioOptions msg -> RadioOptions msg
setName name opts =
    { opts | name = name }


setTitle : String -> RadioOptions msg -> RadioOptions msg
setTitle title opts =
    { opts | title = title }


setSubtitle : Maybe String -> RadioOptions msg -> RadioOptions msg
setSubtitle subtitle opts =
    { opts | subtitle = subtitle }


setChecked : Bool -> RadioOptions msg -> RadioOptions msg
setChecked checked opts =
    { opts | checked = checked }


setOnCheck : Maybe (Bool -> msg) -> RadioOptions msg -> RadioOptions msg
setOnCheck onCheck opts =
    { opts | onCheck = onCheck }


setAttributes : List (H.Attribute msg) -> RadioOptions msg -> RadioOptions msg
setAttributes attributes opts =
    { opts | attributes = attributes }


group : String -> List (Html msg) -> Html msg
group hiddenTitle children =
    H.fieldset [ A.class "ui-input-radioGroup" ]
        (H.legend
            [ A.class "ui-input-radioGroup__hiddenLegend" ]
            [ H.text hiddenTitle ]
            :: List.map Ui.Group.groupItem children
        )


radio : RadioOptions msg -> Html msg
radio { id, name, title, onCheck, checked, subtitle, attributes } =
    H.div []
        [ H.input
            ([ A.type_ "radio"
             , A.id id
             , A.name name
             , A.class "ui-input-radio__input"
             , maybeOnCheck onCheck
             , A.checked checked
             ]
                ++ attributes
            )
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
