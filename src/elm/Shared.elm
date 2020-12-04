module Shared exposing (Msg, Shared, init, load, update)

import Data.Webshop exposing (FareProduct, TariffZone, UserProfile)
import Environment exposing (Environment)
import Http
import Service.Webshop as WebshopService
import Task


type Msg
    = ReceiveTariffZones (Result Http.Error (List TariffZone))
    | ReceiveFareProducts (Result Http.Error (List FareProduct))
    | ReceiveUserProfiles (Result Http.Error (List UserProfile))


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


load : Environment -> Cmd Msg
load env =
    Cmd.batch
        [ fetchTariffZones env
        , fetchFareProducts env
        , fetchUserProfiles env
        ]


fetchTariffZones : Environment -> Cmd Msg
fetchTariffZones env =
    WebshopService.getTariffZones env
        |> Http.toTask
        |> Task.attempt ReceiveTariffZones


fetchFareProducts : Environment -> Cmd Msg
fetchFareProducts env =
    WebshopService.getFareProducts env
        |> Http.toTask
        |> Task.attempt ReceiveFareProducts


fetchUserProfiles : Environment -> Cmd Msg
fetchUserProfiles env =
    WebshopService.getUserProfiles env
        |> Http.toTask
        |> Task.attempt ReceiveUserProfiles
