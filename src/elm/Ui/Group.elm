module Ui.Group exposing (togglable)

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
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


empty : Html msg
empty =
    H.text ""


maybeHtml : Maybe (Html msg) -> Html msg
maybeHtml maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            empty


maybeText : Maybe String -> Html msg
maybeText maybe =
    case maybe of
        Just a ->
            H.span [] [ H.text a ]

        Nothing ->
            empty


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

        buttonAttributes =
            case onOpenClick of
                Just action ->
                    [ E.onClick action ]

                _ ->
                    []
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.h3 [ A.class "group__header" ]
                    [ H.button
                        (List.append buttonAttributes
                            [ A.classList classListButton
                            , A.disabled disabled
                            , A.attribute "aria-expanded" (stringFromBool open)
                            , A.attribute "aria-controls" regionId
                            , A.id id
                            ]
                        )
                        [ maybeHtml icon
                        , H.div [ A.class "group__headerButton__title" ] [ Ui.Heading.componentWithEl H.span title ]
                        , maybeText value
                        , chevronIcon
                        ]
                    ]
                , H.div
                    [ A.classList classListContent
                    , A.attribute "aria-labelledby" id
                    , A.attribute "role" "region"
                    , A.id regionId
                    ]
                    children
                ]
            ]


stringFromBool : Bool -> String
stringFromBool value =
    if value then
        "true"

    else
        "false"
