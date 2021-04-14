module Ui.Group exposing (groupItem, togglable)

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Events as E
import Html.Extra as Html
import Ui.Heading
import Ui.TextContainer


type alias GroupMetadata msg =
    { title : String
    , id : String
    , icon : Maybe (Html msg)
    , value : Maybe String
    , open : Bool
    , disabled : Bool
    , onOpenClick : Maybe msg
    }


togglable : GroupMetadata msg -> List (Html msg) -> Html msg
togglable { open, disabled, onOpenClick, icon, id, title, value } children =
    let
        classList =
            [ ( "group", True )
            , ( "group--open", open )
            , ( "group--disabled", disabled )
            ]

        classListContent =
            [ ( "group__content", True )
            , ( "group__content--open", open )
            ]

        classListButton =
            [ ( "group__headerButton", True )
            , ( "group__headerButton--disabled", disabled )
            ]

        chevronIcon =
            if open then
                Fragment.Icon.upArrow

            else
                Fragment.Icon.downArrow

        regionId =
            id ++ "region"
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.h3 [ A.class "group__header" ]
                    [ H.button
                        [ A.classList classListButton
                        , A.disabled disabled
                        , A.attribute "aria-expanded" (boolAsString open)
                        , A.attribute "aria-controls" regionId
                        , A.id id
                        , Attr.attributeMaybe (\action -> E.onClick action) onOpenClick
                        ]
                        [ viewMaybe icon
                        , H.div [ A.class "group__headerButton__title" ] [ Ui.Heading.componentWithEl H.span title ]
                        , viewMaybeText value
                        , chevronIcon
                        ]
                    ]
                    |> groupItem
                , H.div
                    [ A.classList classListContent
                    , A.attribute "aria-labelledby" id
                    , Attr.role "region"
                    , A.id regionId
                    , Attr.attributeIf (not open) (A.attribute "inert" "true")
                    ]
                    (children
                        |> List.map groupItem
                    )
                ]
            ]


groupItem : Html msg -> Html msg
groupItem =
    List.singleton >> H.div [ A.class "group__item" ]


boolAsString : Bool -> String
boolAsString b =
    if b then
        "true"

    else
        "false"


viewMaybe : Maybe (Html msg) -> Html msg
viewMaybe =
    Html.viewMaybe identity


viewMaybeText : Maybe String -> Html msg
viewMaybeText value =
    let
        spanNode =
            H.text >> List.singleton >> H.span []
    in
        viewMaybe (Maybe.map spanNode value)
