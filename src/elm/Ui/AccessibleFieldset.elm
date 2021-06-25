module Ui.AccessibleFieldset exposing (view)

import Html as H exposing (Html)
import Html.Attributes as A


view : String -> List (Html msg) -> Html msg
view accessibilityName children =
    H.fieldset [ A.class "ui-accessibleFieldset" ]
        [ H.legend
            [ A.class "ui-accessibleFieldset__legend" ]
            [ H.text accessibilityName ]
        , H.div [ A.class "ui-accessibleFieldset__container" ]
            children
        ]
