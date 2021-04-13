module Ui.Group exposing (togglable)

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Svg exposing (metadata)
import Ui.Heading
import Ui.TextContainer


type alias GroupMetadata msg =
    { title : String
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


togglable : GroupMetadata msg -> List (Html msg) -> Html msg
togglable metadata children =
    let
        classList =
            [ ( "group", True )
            , ( "group--open", metadata.open )
            , ( "group--disabled", metadata.disabled )
            ]

        classListContent =
            [ ( "group__content", True )
            , ( "group__content--open", metadata.open )
            ]

        classListButton =
            [ ( "group__headerButton", True )
            , ( "group__headerButton--disabled", metadata.disabled )
            ]

        chevronIcon =
            if metadata.open then
                Fragment.Icon.upArrow

            else
                Fragment.Icon.downArrow

        buttonAttributes =
            case metadata.onOpenClick of
                Just action ->
                    [ E.onClick action ]

                _ ->
                    []
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.header [ A.class "group__header" ]
                    [ H.button
                        (List.append buttonAttributes
                            [ A.classList classListButton
                            , A.disabled metadata.disabled
                            ]
                        )
                        [ maybeHtml metadata.icon
                        , H.div [ A.class "group__headerButton__title" ] [ Ui.Heading.component metadata.title ]
                        , chevronIcon
                        ]
                    ]
                , H.div [ A.classList classListContent ]
                    children
                ]
            ]
