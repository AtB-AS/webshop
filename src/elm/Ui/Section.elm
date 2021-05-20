module Ui.Section exposing
    ( Section
    , init
    , setMarginBottom
    , setMarginTop
    , view
    , viewGroup
    , viewHeader
    , viewItem
    , viewLabelItem
    , viewPaddedItem
    , viewWithIcon
    , viewWithOptions
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Ui.LabelItem


type alias Section =
    { marginTop : Bool
    , marginBottom : Bool
    }


init : Section
init =
    { marginTop = False, marginBottom = False }


setMarginTop : Bool -> Section -> Section
setMarginTop margin opts =
    { opts | marginTop = margin }


setMarginBottom : Bool -> Section -> Section
setMarginBottom margin opts =
    { opts | marginBottom = margin }


viewWithOptions : List (Html msg) -> Section -> Html msg
viewWithOptions items options =
    let
        classList =
            [ ( "ui-section", True )
            , ( "ui-section--marginTop", options.marginTop )
            , ( "ui-section--marginBottom", options.marginBottom )
            ]
    in
        H.div [ A.classList classList ] (List.map (List.singleton >> internalItem) items)


view : List (Html msg) -> Html msg
view children =
    viewWithOptions children init


viewGroup : String -> List (Html msg) -> Html msg
viewGroup title children =
    viewItem
        (H.h2 [ A.class "ui-section__headerTitle ui-section__headerTitle--padded typo-heading__component" ] [ H.text title ]
            :: children
        )


viewWithIcon : Html msg -> List (Html msg) -> Html msg
viewWithIcon icon children =
    H.div [ A.class "ui-section__itemWithIcon" ]
        [ H.div [ A.class "ui-section__itemWithIcon__icon" ] [ icon ], H.div [ A.class "ui-section__itemWithIcon__content" ] children ]


viewPaddedItem : List (Html msg) -> Html msg
viewPaddedItem =
    H.div [ A.class "ui-section__item ui-section__item--padded" ]


viewItem : List (Html msg) -> Html msg
viewItem =
    H.div [ A.class "ui-section__item" ]


viewLabelItem : String -> List (Html msg) -> Html msg
viewLabelItem label children =
    viewPaddedItem
        [ Ui.LabelItem.view label children ]


viewHeader : String -> Html msg
viewHeader title =
    viewPaddedItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]


internalItem : List (Html msg) -> Html msg
internalItem children =
    H.div [ A.class "ui-section__child" ] children
