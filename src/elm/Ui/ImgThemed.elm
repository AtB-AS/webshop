module Ui.ImgThemed exposing (view, viewMono)

import Html as H exposing (Attribute, Html)


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    H.node "atb-img-themed"


viewMono : List (Attribute msg) -> List (Html msg) -> Html msg
viewMono =
    H.node "atb-mono-icon"
