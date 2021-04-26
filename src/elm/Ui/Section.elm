module Ui.Section exposing
    ( Section
    , sectionWithOptions
    , view
    , viewHeader
    , viewItem
    , viewLabelItem
    , viewPaddedItem
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))



-- sectionItem : Html msg -> Html msg
-- sectionItem =
--     List.singleton >> H.div [ A.class "ui-section__item" ]


type alias Section =
    { marginTop : Bool
    , marginBottom : Bool
    }


sectionWithOptions : Section -> List (Html msg) -> Html msg
sectionWithOptions options items =
    let
        classList =
            [ ( "ui-section", True )
            , ( "ui-section--marginTop", options.marginTop )
            , ( "ui-section--marginBottom", options.marginBottom )
            ]
    in
        H.div [ A.classList classList ] (List.map (List.singleton >> internalItem) items)


view : List (Html msg) -> Html msg
view =
    sectionWithOptions { marginTop = False, marginBottom = False }


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
            [ Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )


viewHeader : String -> Html msg
viewHeader title =
    viewPaddedItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]


internalItem : List (Html msg) -> Html msg
internalItem children =
    H.div [ A.class "ui-section__child" ] children
