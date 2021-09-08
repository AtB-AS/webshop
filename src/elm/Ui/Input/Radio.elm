module Ui.Input.Radio exposing
    ( Radio
    , init
    , setAriaLabel
    , setAttributes
    , setChecked
    , setIcon
    , setId
    , setName
    , setOnCheck
    , setSubtitle
    , setTitle
    , view
    , viewGroup
    , viewLabelGroup
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Events as E
import Html.Extra
import Ui.Group
import Ui.ScreenReaderText as SR
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))
import Util.Maybe as MaybeUtil


type alias Radio msg =
    { id : String
    , name : String
    , title : String
    , subtitle : Maybe String
    , ariaLabel : Maybe String
    , icon : Maybe (Html msg)
    , checked : Bool
    , onCheck : Maybe msg
    , attributes : List (H.Attribute msg)
    }


init : String -> Radio msg
init id =
    { id = id
    , name = ""
    , title = ""
    , subtitle = Nothing
    , ariaLabel = Nothing
    , icon = Nothing
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


setAriaLabel : Maybe String -> Radio msg -> Radio msg
setAriaLabel ariaLabel opts =
    { opts | ariaLabel = ariaLabel }


setIcon : Maybe (Html msg) -> Radio msg -> Radio msg
setIcon icon opts =
    { opts | icon = icon }


setChecked : Bool -> Radio msg -> Radio msg
setChecked checked opts =
    { opts | checked = checked }


setOnCheck : Maybe msg -> Radio msg -> Radio msg
setOnCheck onCheck opts =
    { opts | onCheck = onCheck }


setAttributes : List (H.Attribute msg) -> Radio msg -> Radio msg
setAttributes attributes opts =
    { opts | attributes = attributes }


viewGroup : String -> List (Html msg) -> Html msg
viewGroup hiddenTitle children =
    H.fieldset [ A.class "ui-input-radioGroup" ]
        (H.legend
            [ A.class "ui-input-radioGroup__legend ui-input-radioGroup__legend--hidden"
            , A.attribute "aria-hidden" "true"
            ]
            [ H.text hiddenTitle ]
            :: List.map Ui.Group.viewItem children
        )


viewLabelGroup : String -> List (Html msg) -> Html msg
viewLabelGroup title children =
    H.fieldset [ A.class "ui-input-radioGroup" ]
        (H.legend
            [ A.class "ui-input-radioGroup__legend" ]
            [ Text.textContainer H.span (Just Text.SecondaryColor) <| Text.Tertiary [ H.text title ] ]
            :: List.map Ui.Group.viewItem children
        )


view : Radio msg -> Html msg
view { id, name, title, icon, onCheck, checked, subtitle, ariaLabel, attributes } =
    let
        labelId =
            id ++ "-label"

        defaultAriaLabel =
            title ++ MaybeUtil.mapWithDefault ((++) ", ") "" subtitle
    in
        H.div []
            [ H.input
                ([ A.type_ "radio"
                 , A.id id
                 , A.name name
                 , A.class "ui-input-radio__input"
                 , Attr.attributeMaybe (always >> E.onCheck) onCheck
                 , A.checked checked
                 , A.attribute "aria-labelledby" labelId
                 , A.attribute "aria-hidden" "true"
                 ]
                    ++ attributes
                )
                []
            , H.label
                [ A.for id
                , A.class "ui-input-radio"
                , A.attribute "aria-labelledby" labelId
                , A.attribute "role" "radio"
                , A.attribute "aria-checked" (boolToString checked)
                ]
                [ H.div [ A.class "ui-input-radio__content", A.attribute "aria-hidden" "true" ]
                    [ SR.onlyReadWithId (Maybe.withDefault defaultAriaLabel ariaLabel) labelId
                    , H.span [ A.class "ui-input-radio__box" ] []
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
                    , Html.Extra.viewMaybe
                        (\t ->
                            H.span [ A.class "ui-input-radio__icon" ]
                                [ t ]
                        )
                        icon
                    ]
                ]
            ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"
