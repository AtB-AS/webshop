module Shared exposing (Msg, Shared, init, subscriptions, update)

import Data.RefData exposing (Consent, FareProduct, Limitation, ProductType(..), TariffZone, UserProfile, UserType)
import Data.RemoteConfig exposing (RemoteConfig)
import List exposing (product)
import List.Extra
import Service.Misc as MiscService exposing (Profile)
import Service.RefData as RefDataService
import Service.RemoteConfig as RCConfig


type Msg
    = ReceiveTariffZones (Result () (List TariffZone))
    | ReceiveFareProducts (Result () (List FareProduct))
    | ReceiveUserProfiles (Result () (List UserProfile))
    | ReceiveConsents (Result () (List Consent))
    | ReceiveRemoteConfig (Result () RemoteConfig)
    | ProfileChange (Maybe Profile)


type alias Shared =
    { tariffZones : List TariffZone
    , fareProducts : List FareProduct

    -- Available for webshop
    , availableFareProducts : List FareProduct
    , userProfiles : List UserProfile
    , remoteConfig : RemoteConfig
    , productLimitations : List Limitation
    , consents : List Consent
    , profile : Maybe Profile
    }


init : Shared
init =
    { tariffZones = []
    , fareProducts = []
    , availableFareProducts = []
    , userProfiles = []
    , productLimitations = []
    , consents = []
    , profile = Nothing
    , remoteConfig = RCConfig.init
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
                    { model
                        | fareProducts = value
                        , availableFareProducts = List.filter (.type_ >> (==) ProductTypePeriod) value
                        , productLimitations = getMappedLimitations value model.userProfiles
                    }

                Err _ ->
                    model

        ReceiveUserProfiles result ->
            case result of
                Ok value ->
                    { model
                        | userProfiles = value
                        , productLimitations = getMappedLimitations model.fareProducts value
                    }

                Err _ ->
                    model

        ReceiveConsents result ->
            case result of
                Ok value ->
                    { model | consents = value }

                Err _ ->
                    model

        ReceiveRemoteConfig result ->
            case result of
                Ok value ->
                    { model | remoteConfig = value }

                Err _ ->
                    model

        ProfileChange profile ->
            { model | profile = profile }


getMappedLimitations : List FareProduct -> List UserProfile -> List Limitation
getMappedLimitations products userProfiles =
    let
        userIdToType : String -> Maybe UserType
        userIdToType id =
            userProfiles |> List.Extra.find (.id >> (==) id) |> Maybe.map .userType
    in
        products
            |> List.map
                (\product ->
                    { productId = product.id
                    , limitations = List.filterMap userIdToType product.limitations
                    }
                )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ RefDataService.onTariffZones ReceiveTariffZones
        , RefDataService.onFareProducts ReceiveFareProducts
        , RefDataService.onUserProfiles ReceiveUserProfiles
        , RefDataService.onConsents ReceiveConsents
        , RCConfig.onRemoteConfig ReceiveRemoteConfig
        , MiscService.onProfileChange ProfileChange
        ]
