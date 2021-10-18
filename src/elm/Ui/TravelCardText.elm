module Ui.TravelCardText exposing (view, viewMaybeTravelCard)

import Html as H exposing (Html)
import Html.Attributes as A
import Util.TravelCard


view : Int -> Html msg
view id =
    H.span [ A.class "ui-travelCardText", A.attribute "role" "complementary", A.attribute "aria-label" <| "Gjeldende reisekort tall: " ++ String.fromInt id ]
        [ H.span [ A.class "ui-travelCardText__faded", A.attribute "aria-hidden" "true", A.attribute "role" "none" ] [ H.text "**** **" ]
        , H.text <| Util.TravelCard.formatSignificant <| String.fromInt id
        , H.span [ A.class "ui-travelCardText__faded", A.attribute "aria-hidden" "true", A.attribute "role" "none" ] [ H.text "*" ]
        ]


viewMaybeTravelCard : Maybe { a | id : Int } -> Html msg
viewMaybeTravelCard travelCard =
    case travelCard of
        Just { id } ->
            view id

        _ ->
            H.span [] [ H.text "(Ikke satt)" ]
