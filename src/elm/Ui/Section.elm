module Ui.Section exposing
    ( SectionOptions
    , labelItem
    , section
    , sectionGenericItem
    , sectionHeader
    , sectionWithOptions
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))



-- sectionItem : Html msg -> Html msg
-- sectionItem =
--     List.singleton >> H.div [ A.class "ui-section__item" ]


type alias SectionOptions =
    { marginTop : Bool
    , marginBottom : Bool
    }


sectionWithOptions : SectionOptions -> List (Html msg) -> Html msg
sectionWithOptions options items =
    let
        classList =
            [ ( "ui-section", True )
            , ( "ui-section--marginTop", options.marginTop )
            , ( "ui-section--marginBottom", options.marginBottom )
            ]
    in
        H.div [ A.classList classList ] (List.map (List.singleton >> internalItem) items)


section : List (Html msg) -> Html msg
section =
    sectionWithOptions { marginTop = False, marginBottom = False }


sectionGenericItem : List (Html msg) -> Html msg
sectionGenericItem =
    H.div [ A.class "ui-section__item" ]


labelItem : String -> List (Html msg) -> Html msg
labelItem label children =
    H.div [ A.class "ui-section__item" ]
        (H.div [ A.class "ui-section__item__label" ]
            [ Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )


sectionHeader : String -> Html msg
sectionHeader title =
    sectionGenericItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]


internalItem : List (Html msg) -> Html msg
internalItem children =
    H.div [ A.class "ui-section__child" ] children
