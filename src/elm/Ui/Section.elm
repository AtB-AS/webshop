module Ui.Section exposing
    ( Section
    , sectionWithOptions
    , view
    , viewGenericItem
    , viewHeader
    , viewLabelItem
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


viewGenericItem : List (Html msg) -> Html msg
viewGenericItem =
    H.div [ A.class "ui-section__item" ]


viewLabelItem : String -> List (Html msg) -> Html msg
viewLabelItem label children =
    H.div [ A.class "ui-section__item" ]
        (H.div [ A.class "ui-section__item__label" ]
            [ Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )


viewHeader : String -> Html msg
viewHeader title =
    viewGenericItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]


internalItem : List (Html msg) -> Html msg
internalItem children =
    H.div [ A.class "ui-section__child" ] children
