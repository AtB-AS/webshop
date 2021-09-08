module Ui.Group exposing (view, viewItem)

import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Events as E
import Html.Extra
import Ui.LabelItem
import Ui.ScreenReaderText as SR
import Ui.TextContainer


type alias Group msg =
    { title : String
    , id : String
    , editTextSuffix : String
    , icon : Html msg
    , value : Maybe String
    , open : Bool
    , readonly : Bool
    , onOpenClick : Maybe msg
    }


view : Group msg -> List (Html msg) -> Html msg
view { open, readonly, onOpenClick, icon, id, editTextSuffix, title, value } children =
    let
        classListContent =
            [ ( "ui-group__content", True )
            , ( "ui-group__content--open", open )
            ]

        editIcon =
            Icon.viewMonochrome Icon.edit

        editText =
            "Endre " ++ editTextSuffix

        editTextComponent =
            if readonly then
                Html.Extra.nothing

            else if not open then
                H.span [ A.class "ui-group__headerButton__content__editText" ]
                    [ H.span [ A.class "ui-group__headerButton__content__editText__text" ] [ H.text editText ]
                    , editIcon
                    ]

            else
                Icon.upArrow

        ariaLabel =
            title ++ ", " ++ Maybe.withDefault "" value ++ ", " ++ editText

        regionId =
            id ++ "region"
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.class "ui-group" ]
                [ H.div [ A.class "ui-group__icon" ] [ Icon.viewLargeMonochrome icon ]
                , H.div [ A.class "ui-group__innerContainer" ]
                    [ H.button
                        [ A.class "ui-group__headerButton"
                        , A.disabled readonly
                        , A.attribute "aria-expanded" (boolAsString open)
                        , A.attribute "aria-controls" regionId
                        , A.id id
                        , Attr.attributeMaybe (\action -> E.onClick action) onOpenClick
                        ]
                        [ SR.onlyRead ariaLabel
                        , H.div [ A.class "ui-group__headerButton__content", A.attribute "aria-hidden" "true" ]
                            [ Ui.LabelItem.viewInline title [ H.text <| Maybe.withDefault "" value ]
                            , editTextComponent
                            ]
                        ]
                    , H.div
                        [ A.classList classListContent
                        , A.attribute "aria-labelledby" id
                        , Attr.role "region"
                        , A.id regionId
                        , Attr.attributeIf (not open) (A.attribute "inert" "true")
                        ]
                        (children
                            |> List.map viewItem
                        )
                    ]
                ]
            ]


viewItem : Html msg -> Html msg
viewItem =
    List.singleton >> H.div [ A.class "ui-group__item" ]


boolAsString : Bool -> String
boolAsString b =
    if b then
        "true"

    else
        "false"
