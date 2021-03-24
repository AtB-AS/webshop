module Shared exposing (Msg, Shared, init, subscriptions, update)

import Data.RefData exposing (FareProduct, TariffZone, UserProfile)
import Service.RefData as RefDataService


type Msg
    = ReceiveTariffZones (Result () (List TariffZone))
    | ReceiveFareProducts (Result () (List FareProduct))
    | ReceiveUserProfiles (Result () (List UserProfile))


type alias Shared =
    { tariffZones : List TariffZone
    , fareProducts : List FareProduct
    , userProfiles : List UserProfile
    }


init : Shared
init =
    { tariffZones = []
    , fareProducts = []
    , userProfiles = []
    }


update : Msg -> Shared -> Shared
update msg model =
    case msg of
        ReceiveTariffZones result ->
            case result of
                Ok value ->
                    { model | tariffZones = value }

                Err _ ->
                    model

        ReceiveFareProducts result ->
            case result of
                Ok value ->
                    { model | fareProducts = value }

                Err _ ->
                    model

        ReceiveUserProfiles result ->
            case result of
                Ok value ->
                    { model | userProfiles = value }

                Err _ ->
                    model


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ RefDataService.onTariffZones ReceiveTariffZones
        , RefDataService.onFareProducts ReceiveFareProducts
        , RefDataService.onUserProfiles ReceiveUserProfiles
        ]
