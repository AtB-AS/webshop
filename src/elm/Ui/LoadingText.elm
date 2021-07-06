module Ui.LoadingText exposing (view)

import Html as H exposing (Html)
import Html.Attributes as A


view : String -> String -> Html msg
view height width =
    H.div
        [ A.class "ui-loadingText"
        , A.style "height" height
        , A.style "width" width
        , A.attribute "role" "progressbar"
        , A.attribute "aria-valuetext" "Laster inn"
        , A.attribute "aria-label" "Laster"
        ]
        []
