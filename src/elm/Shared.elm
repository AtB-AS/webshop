module Shared exposing (Msg, Shared, init, subscriptions, update)

import Data.RefData exposing (FareProduct, Limitation, TariffZone, UserProfile, UserType)
import List exposing (product)
import List.Extra
import Service.Misc as MiscService exposing (Profile)
import Service.RefData as RefDataService


type Msg
    = ReceiveTariffZones (Result () (List TariffZone))
    | ReceiveFareProducts (Result () (List FareProduct))
    | ReceiveUserProfiles (Result () (List UserProfile))
    | ProfileChange (Maybe Profile)


type alias Shared =
    { tariffZones : List TariffZone
    , fareProducts : List FareProduct
    , userProfiles : List UserProfile
    , productLimitations : List Limitation
    , profile : Maybe Profile
    }


init : Shared
init =
    { tariffZones = []
    , fareProducts = []
    , userProfiles = []
    , productLimitations = []
    , profile = Nothing
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
        , MiscService.onProfileChange ProfileChange
        ]
