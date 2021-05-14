module Ui.LabelItem exposing
    ( view
    , viewCompact
    , viewHorizontal
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


view : String -> List (Html msg) -> Html msg
view =
    internalView [] []


viewCompact : String -> List (Html msg) -> Html msg
viewCompact =
    internalView [] [ A.class "ui-labelItem__label--compact" ]


viewHorizontal : String -> List (Html msg) -> Html msg
viewHorizontal =
    internalView [ A.class "ui-labelItem--horizontal" ] []



-- Internal


internalView : List (Attribute msg) -> List (Attribute msg) -> String -> List (Html msg) -> Html msg
internalView attrs attrsChild label children =
    H.div (A.class "ui-labelItem" :: attrs)
        (H.div (A.class "ui-labelItem__label" :: attrsChild)
            [ Text.textContainer (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )
