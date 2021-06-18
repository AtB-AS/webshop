module Ui.Input.Radio exposing
    ( Radio
    , init
    , setAttributes
    , setChecked
    , setId
    , setName
    , setOnCheck
    , setSubtitle
    , setTitle
    , view
    , viewGroup
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Ui.Button exposing (ButtonMode(..))
import Ui.Group
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias Radio msg =
    { id : String
    , name : String
    , title : String
    , subtitle : Maybe String
    , checked : Bool
    , onCheck : Maybe (Bool -> msg)
    , attributes : List (H.Attribute msg)
    }


init : String -> Radio msg
init id =
    { id = id
    , name = ""
    , title = ""
    , subtitle = Nothing
    , checked = False
    , onCheck = Nothing
    , attributes = []
    }


setId : String -> Radio msg -> Radio msg
setId id opts =
    { opts | id = id }


setName : String -> Radio msg -> Radio msg
setName name opts =
    { opts | name = name }


setTitle : String -> Radio msg -> Radio msg
setTitle title opts =
    { opts | title = title }


setSubtitle : Maybe String -> Radio msg -> Radio msg
setSubtitle subtitle opts =
    { opts | subtitle = subtitle }


setChecked : Bool -> Radio msg -> Radio msg
setChecked checked opts =
    { opts | checked = checked }


setOnCheck : Maybe (Bool -> msg) -> Radio msg -> Radio msg
setOnCheck onCheck opts =
    { opts | onCheck = onCheck }


setAttributes : List (H.Attribute msg) -> Radio msg -> Radio msg
setAttributes attributes opts =
    { opts | attributes = attributes }


viewGroup : String -> List (Html msg) -> Html msg
viewGroup hiddenTitle children =
    H.fieldset [ A.class "ui-input-radioGroup" ]
        (H.legend
            [ A.class "ui-input-radioGroup__hiddenLegend" ]
            [ H.text hiddenTitle ]
            :: List.map Ui.Group.viewItem children
        )


view : Radio msg -> Html msg
view { id, name, title, onCheck, checked, subtitle, attributes } =
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
            [ H.span [ A.class "ui-input-radio__box" ] []
            , H.span [ A.class "ui-input-radio__title" ]
                [ H.span [] [ H.text title ]
                , Html.Extra.viewMaybe
                    (\t ->
                        H.span
                            [ A.class "ui-input-radio__subtitle"
                            ]
                            [ Text.textContainer H.span (Just Text.SecondaryColor) <| Text.Tertiary [ H.text t ] ]
                    )
                    subtitle
                ]
            ]
        ]


maybeOnCheck : Maybe (Bool -> msg) -> Attribute msg
maybeOnCheck maybeAction =
    case maybeAction of
        Just action ->
            E.onCheck action

        Nothing ->
            A.classList []
