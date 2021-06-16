module Ui.LabelItem exposing
    ( view
    , viewCompact
    , viewCompactInline
    , viewHorizontal
    , viewHorizontalInline
    , viewInline
    )

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias Element msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


view : String -> List (Html msg) -> Html msg
view =
    internalView H.div [] []


viewCompact : String -> List (Html msg) -> Html msg
viewCompact =
    internalView H.div [] [ A.class "ui-labelItem__label--compact" ]


viewHorizontal : String -> List (Html msg) -> Html msg
viewHorizontal =
    internalView H.div [ A.class "ui-labelItem--horizontal" ] []


viewInline : String -> List (Html msg) -> Html msg
viewInline =
    internalView H.span [] []


viewCompactInline : String -> List (Html msg) -> Html msg
viewCompactInline =
    internalView H.span [] [ A.class "ui-labelItem__label--compact" ]


viewHorizontalInline : String -> List (Html msg) -> Html msg
viewHorizontalInline =
    internalView H.span [ A.class "ui-labelItem--horizontal" ] []



-- Internal


internalView : Element msg -> List (Attribute msg) -> List (Attribute msg) -> String -> List (Html msg) -> Html msg
internalView el attrs attrsChild label children =
    el (A.class "ui-labelItem" :: attrs)
        (el (A.class "ui-labelItem__label" :: attrsChild)
            [ Text.textContainer el (Just Text.SecondaryColor) <| Text.Tertiary [ H.text label ]
            ]
            :: children
        )
