module Ui.Expandable exposing (view, viewItem)

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Events as E
import Html.Extra
import Ui.Heading
import Ui.TextContainer


type alias Expandable msg =
    { title : String
    , id : String
    , icon : Maybe (Html msg)
    , open : Bool
    , onOpenClick : Maybe msg
    }


view : Expandable msg -> List (Html msg) -> Html msg
view { open, onOpenClick, icon, id, title } children =
    let
        classList =
            [ ( "ui-expandable", True )
            , ( "ui-expandable--open", open )
            ]

        classListContent =
            [ ( "ui-expandable__content", True )
            , ( "ui-expandable__content--open", open )
            ]

        chevronIcon =
            if open then
                Fragment.Icon.upArrow

            else
                Fragment.Icon.downArrow

        expandText =
            if open then
                "Skjul"

            else
                "Vis"

        regionId =
            id ++ "region"
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.h3 [ A.class "ui-expandable__header" ]
                    [ H.button
                        [ A.class "ui-expandable__headerButton"
                        , A.attribute "aria-expanded" (boolAsString open)
                        , A.attribute "aria-controls" regionId
                        , A.id id
                        , Attr.attributeMaybe (\action -> E.onClick action) onOpenClick
                        ]
                        [ viewMaybe icon
                        , H.span [ A.class "ui-expandable__headerButton__title" ] [ Ui.Heading.componentWithEl H.span title ]
                        , H.span [ A.class "ui-expandable__headerButton__expandText" ] [ H.text expandText ]
                        , chevronIcon
                        ]
                    ]
                    |> viewItem
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


viewItem : Html msg -> Html msg
viewItem =
    List.singleton >> H.div [ A.class "ui-expandable__item" ]


boolAsString : Bool -> String
boolAsString b =
    if b then
        "true"

    else
        "false"


viewMaybe : Maybe (Html msg) -> Html msg
viewMaybe =
    Html.Extra.viewMaybe identity
