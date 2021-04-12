module Ui.Section exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A



-- sectionItem : Html msg -> Html msg
-- sectionItem =
--     List.singleton >> H.div [ A.class "section__item" ]


type alias SectionOptions =
    { marginTop : Bool
    , marginBottom : Bool
    }


sectionWithOptions : SectionOptions -> List (Html msg) -> Html msg
sectionWithOptions options items =
    let
        classList =
            [ ( "section", True )
            , ( "section--marginTop", options.marginTop )
            , ( "section--marginBottom", options.marginBottom )
            ]
    in
        H.div [ A.classList classList ] items


section : List (Html msg) -> Html msg
section =
    sectionWithOptions { marginTop = False, marginBottom = False }


sectionGenericItem : List (Html msg) -> Html msg
sectionGenericItem children =
    H.div [ A.class "section__item" ] children


sectionHeader : String -> Html msg
sectionHeader title =
    sectionGenericItem [ H.h2 [ A.class "sectionHeaderTitle typo-heading__component" ] [ H.text title ] ]
