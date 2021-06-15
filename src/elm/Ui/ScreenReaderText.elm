module Ui.ScreenReaderText exposing (view)

import Html as H exposing (Html)
import Html.Attributes as A


view : String -> Html msg
view text =
    H.span [ A.class "ui-srOnly" ] [ H.text text ]
