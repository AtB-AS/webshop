module Ui.TravelCardText exposing (view, viewMaybeTravelCard)

import Html as H exposing (Html)
import Html.Attributes as A
import Util.TravelCard


view : Int -> Html msg
view id =
    H.span [ A.class "ui-travelCardText" ]
        [ H.span [ A.class "ui-travelCardText__faded" ] [ H.text "XXX XX" ]
        , H.text <| Util.TravelCard.formatSignificant <| String.fromInt id
        , H.span [ A.class "ui-travelCardText__faded" ] [ H.text "X" ]
        ]


viewMaybeTravelCard : Maybe { a | id : Int } -> Html msg
viewMaybeTravelCard travelCard =
    case travelCard of
        Just { id } ->
            view id

        _ ->
            H.span [] [ H.text "(Ikke satt)" ]
