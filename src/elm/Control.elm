module Control exposing (actionButton, linkButton)

import Html as H exposing (Attribute, Html)
import Html.Attributes as A


{-| Base function for creating buttons. Used by other button functions
-}
baseButton : String -> String -> List (Attribute msg) -> Html msg
baseButton style label attributes =
    H.button (A.class style :: attributes) [ H.text label ]


{-| Create an action button.

     actionButton "Click me" [ onClick Click ]

-}
actionButton : String -> List (Attribute msg) -> Html msg
actionButton label attributes =
    baseButton "action-button" label attributes


{-| Create a link button.

     linkButton "Click me" [ onClick Click ]

-}
linkButton : String -> List (Attribute msg) -> Html msg
linkButton label attributes =
    baseButton "link-button" label attributes
