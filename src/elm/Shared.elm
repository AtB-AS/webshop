module Shared exposing (Msg, Shared, filterAvailableOnProductType, hasCarnetTickets, hasNonCarnetTickets, init, subscriptions, update)

import Data.FareContract exposing (TravelRight(..))
import Data.PaymentType exposing (PaymentType)
import Data.RefData exposing (Consent, DistributionChannel(..), FareProduct, Limitation, ProductType(..), TariffZone, UserProfile, UserType)
import List exposing (product)
import List.Extra
import Service.Misc as MiscService exposing (Profile)
import Service.RefData as RefDataService
import Service.RemoteConfig as RemoteConfigService


type Msg
    = ReceiveTariffZones (Result () (List TariffZone))
    | ReceiveFareProducts (Result () (List FareProduct))
    | ReceiveUserProfiles (Result () (List UserProfile))
    | ReceiveConsents (Result () (List Consent))
    | ReceivePaymentTypes (Result () (List PaymentType))
    | ReceiveVatPercent Float
    | ProfileChange (Maybe Profile)


type alias Shared =
    { tariffZones : List TariffZone
    , fareProducts : List FareProduct

    -- Available for webshop
    , availableFareProducts : List FareProduct
    , userProfiles : List UserProfile
    , vatPercent : Float
    , productLimitations : List Limitation
    , consents : List Consent
    , paymentTypes : List PaymentType
    , profile : Maybe Profile
    , availableCarnetProducts : List FareProduct
    }


init : Shared
init =
    { tariffZones = []
    , fareProducts = []
    , availableFareProducts = []
    , availableCarnetProducts = []
    , userProfiles = []
    , productLimitations = []
    , consents = []
    , paymentTypes = []
    , profile = Nothing
    , vatPercent = 12
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
                        , availableFareProducts = filterAvailableOnProductType [ ProductTypeSingle, ProductTypePeriod ] value
                        , availableCarnetProducts = filterAvailableOnProductType [ ProductTypeCarnet ] value
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

        ReceivePaymentTypes result ->
            case result of
                Ok value ->
                    { model | paymentTypes = value }

                Err _ ->
                    model

        ReceiveVatPercent value ->
            { model | vatPercent = value }

        ProfileChange profile ->
            { model | profile = profile }


hasDistributionWeb : FareProduct -> Bool
hasDistributionWeb =
    .distributionChannel >> List.member DistributionChannelWeb


isAnyProductType : List ProductType -> FareProduct -> Bool
isAnyProductType types product =
    List.member product.type_ types


filterAvailableOnProductType : List ProductType -> List FareProduct -> List FareProduct
filterAvailableOnProductType types products =
    products
        |> List.filter hasDistributionWeb
        |> List.filter (isAnyProductType types)


{-| Check if we have period tickets valid for Web
-}
hasNonCarnetTickets : Shared -> Bool
hasNonCarnetTickets shared =
    not <| List.isEmpty shared.availableFareProducts


{-| Check if we have carnet tickets valid for Web
-}
hasCarnetTickets : Shared -> Bool
hasCarnetTickets shared =
    not <| List.isEmpty shared.availableCarnetProducts


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ RefDataService.onTariffZones ReceiveTariffZones
        , RefDataService.onFareProducts ReceiveFareProducts
        , RefDataService.onUserProfiles ReceiveUserProfiles
        , RefDataService.onConsents ReceiveConsents
        , RefDataService.onPaymentTypes ReceivePaymentTypes
        , RemoteConfigService.onVatPercent ReceiveVatPercent
        , MiscService.onProfileChange ProfileChange
        ]



-- INTERNALS


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
