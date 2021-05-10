module GlobalActions exposing
    ( GlobalAction(..)
    , map
    )

import Data.Ticket exposing (ActiveReservation)
import Notification exposing (Notification)
import Route as Route


type GlobalAction msg
    = RouteTo Route.Route
    | ShowNotification (Notification msg)
    | SetCustomerNumber Int
    | OpenShop
    | OpenEditTravelCard
    | CloseShop
    | AddActiveReservation ActiveReservation
    | FocusItem (Maybe String)
    | Logout


map : (a -> msg) -> GlobalAction a -> GlobalAction msg
map f ga =
    case ga of
        RouteTo route ->
            RouteTo route

        ShowNotification notification ->
            ShowNotification <| Notification.map f notification

        SetCustomerNumber number ->
            SetCustomerNumber number

        OpenShop ->
            OpenShop

        OpenEditTravelCard ->
            OpenEditTravelCard

        CloseShop ->
            CloseShop

        AddActiveReservation reservation ->
            AddActiveReservation reservation

        Logout ->
            Logout

        FocusItem id ->
            FocusItem id
