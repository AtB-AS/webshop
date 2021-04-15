module Ui.Section exposing
    ( SectionOptions
    , section
    , sectionGenericItem
    , sectionHeader
    , sectionWithOptions
    )

import Html as H exposing (Html)
import Html.Attributes as A



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
        H.div [ A.classList classList ] items


section : List (Html msg) -> Html msg
section =
    sectionWithOptions { marginTop = False, marginBottom = False }


sectionGenericItem : List (Html msg) -> Html msg
sectionGenericItem children =
    H.div [ A.class "ui-section__item" ] children


sectionHeader : String -> Html msg
sectionHeader title =
    sectionGenericItem [ H.h2 [ A.class "ui-section__headerTitle typo-heading__component" ] [ H.text title ] ]
