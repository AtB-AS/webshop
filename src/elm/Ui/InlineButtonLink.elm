module Ui.InlineButtonLink exposing (view)

import Html as H exposing (Attribute, Html)
import Html.Attributes as A


view : List (Attribute msg) -> List (Html msg) -> Html msg
view attrs =
    H.button ([ A.class "ui-inlineButtonLink", A.type_ "button" ] ++ attrs)
