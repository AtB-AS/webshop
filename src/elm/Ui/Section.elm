module Ui.Section exposing
    ( Section
    , init
    , setMarginBottom
    , setMarginTop
    , view
    , viewHeader
    , viewItem
    , viewLabelItem
    , viewPaddedItem
    , viewWithOptions
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


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


viewPaddedItem : List (Html msg) -> Html msg
viewPaddedItem =
    H.div [ A.class "ui-section__item ui-section__item--padded" ]


viewItem : List (Html msg) -> Html msg
viewItem =
    H.div [ A.class "ui-section__item" ]


viewLabelItem : String -> List (Html msg) -> Html msg
viewLabelItem label children =
    viewPaddedItem
        (H.div [ A.class "ui-section__item__label" ]
            [ Text.textContainer H.div (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )


viewHeader : String -> Html msg
viewHeader title =
    viewPaddedItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]


internalItem : List (Html msg) -> Html msg
internalItem children =
    H.div [ A.class "ui-section__child" ] children
