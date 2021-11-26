module Ui.ImgThemed exposing (view)

import Html as H exposing (Attribute, Html)


view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    H.node "atb-img-themed"
