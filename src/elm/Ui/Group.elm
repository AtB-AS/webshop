module Ui.Group exposing (view, viewItem)

import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Events as E
import Ui.LabelItem
import Ui.Section
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
        classList =
            [ ( "ui-group", True )
            , ( "ui-group--open", open )
            ]

        classListContent =
            [ ( "ui-group__content", True )
            , ( "ui-group__content--open", open )
            ]

        editIcon =
            Icon.viewMonochrome Icon.edit

        editText =
            if not open then
                H.span [ A.class "ui-group__headerButton__editText" ]
                    [ H.span [ A.class "ui-group__headerButton__editText__text" ] [ H.text <| "Endre " ++ editTextSuffix ]
                    , editIcon
                    ]

            else
                Icon.upArrow

        regionId =
            id ++ "region"
    in
        Ui.TextContainer.primary
            [ if readonly then
                Ui.Section.viewWithIcon (Icon.viewLargeMonochrome icon)
                    [ Ui.Section.viewLabelItem title
                        [ H.text <| Maybe.withDefault "" value
                        ]
                    ]

              else
                H.section
                    [ A.classList classList ]
                    [ H.div [ A.class "ui-group__icon" ] [ Icon.viewLargeMonochrome icon ]
                    , H.div [ A.class "ui-group__innerContainer" ]
                        [ H.h3 [ A.class "ui-group__header" ]
                            [ H.button
                                [ A.class "ui-group__headerButton"
                                , A.disabled readonly
                                , A.attribute "aria-expanded" (boolAsString open)
                                , A.attribute "aria-controls" regionId
                                , A.id id
                                , Attr.attributeMaybe (\action -> E.onClick action) onOpenClick
                                ]
                                [ Ui.LabelItem.viewInline title [ H.text <| Maybe.withDefault "" value ]
                                , editText
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
