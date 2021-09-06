port module Service.RefData exposing
    ( onConsents
    , onFareProducts
    , onPaymentTypes
    , onTariffZones
    , onUserProfiles
    )

import Data.PaymentType as PaymentType exposing (PaymentType)
import Data.RefData exposing (Consent, DistributionChannel(..), FareProduct, LangString(..), ProductType(..), TariffZone, UserProfile, UserType(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP



-- DECODERS


tariffZoneDecoder : Decoder TariffZone
tariffZoneDecoder =
    Decode.succeed TariffZone
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder


fareProductDecoder : Decoder FareProduct
fareProductDecoder =
    Decode.succeed FareProduct
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder
        |> DecodeP.optional "description" langStringDecoder (LangString "" "")
        |> DecodeP.required "type" productTypeDecoder
        |> DecodeP.required "distributionChannel" (Decode.list distributionChannelDecoder)
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)
        |> DecodeP.required "limitations" limitationsDecoder


productTypeDecoder : Decoder ProductType
productTypeDecoder =
    Decode.andThen
        (\userType ->
            case userType of
                "period" ->
                    Decode.succeed ProductTypePeriod

                "single" ->
                    Decode.succeed ProductTypeSingle

                "carnet" ->
                    Decode.succeed ProductTypeCarnet

                _ ->
                    Decode.fail "Invalid product type"
        )
        Decode.string


distributionChannelDecoder : Decoder DistributionChannel
distributionChannelDecoder =
    Decode.andThen
        (\userType ->
            case userType of
                "web" ->
                    Decode.succeed DistributionChannelWeb

                "app" ->
                    Decode.succeed DistributionChannelApp

                _ ->
                    Decode.fail "Invalid distributionChannel"
        )
        Decode.string


userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    Decode.succeed UserProfile
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder
        |> DecodeP.required "description" langStringDecoder
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)
        |> DecodeP.custom
            (Decode.succeed Tuple.pair
                |> DecodeP.optional "minAge" Decode.int 0
                |> DecodeP.optional "maxAge" Decode.int 999
            )
        |> DecodeP.required "userType" userTypeDecoder


langStringDecoder : Decoder LangString
langStringDecoder =
    Decode.succeed LangString
        |> DecodeP.optional "lang" Decode.string ""
        |> DecodeP.optional "value" Decode.string ""


limitationsDecoder : Decoder (List String)
limitationsDecoder =
    Decode.succeed identity
        |> DecodeP.required "userProfileRefs" (Decode.list Decode.string)


userTypeDecoder : Decoder UserType
userTypeDecoder =
    Decode.andThen
        (\userType ->
            case userType of
                1 ->
                    Decode.succeed UserTypeAdult

                2 ->
                    Decode.succeed UserTypeChild

                3 ->
                    Decode.succeed UserTypeInfant

                4 ->
                    Decode.succeed UserTypeSenior

                5 ->
                    Decode.succeed UserTypeStudent

                6 ->
                    Decode.succeed UserTypeYoungPerson

                7 ->
                    Decode.succeed UserTypeSchoolPupil

                8 ->
                    Decode.succeed UserTypeMilitary

                9 ->
                    Decode.succeed UserTypeDisabled

                10 ->
                    Decode.succeed UserTypeDisabledCompanion

                11 ->
                    Decode.succeed UserTypeJobSeeker

                12 ->
                    Decode.succeed UserTypeEmployee

                13 ->
                    Decode.succeed UserTypeAnimal

                14 ->
                    Decode.succeed UserTypeAnyone

                _ ->
                    Decode.fail "Invalid user type"
        )
        Decode.int


consentDecoder : Decoder Consent
consentDecoder =
    Decode.succeed Consent
        |> DecodeP.required "id" Decode.int
        |> DecodeP.required "title" (Decode.dict Decode.string)


paymentTypeDecoder : Decoder PaymentType
paymentTypeDecoder =
    Decode.andThen
        (\value ->
            case PaymentType.fromString value of
                Just paymentType ->
                    Decode.succeed paymentType

                Nothing ->
                    Decode.fail "Invalid payment type"
        )
        Decode.string


onTariffZones : (Result () (List TariffZone) -> msg) -> Sub msg
onTariffZones =
    remoteConfigDecoder tariffZoneDecoder >> remoteConfigTariffZones


onFareProducts : (Result () (List FareProduct) -> msg) -> Sub msg
onFareProducts =
    remoteConfigDecoder fareProductDecoder >> remoteConfigFareProducts


onUserProfiles : (Result () (List UserProfile) -> msg) -> Sub msg
onUserProfiles =
    remoteConfigDecoder userProfileDecoder >> remoteConfigUserProfiles


onConsents : (Result () (List Consent) -> msg) -> Sub msg
onConsents =
    remoteConfigDecoder consentDecoder >> remoteConfigConsents


onPaymentTypes : (Result () (List PaymentType) -> msg) -> Sub msg
onPaymentTypes =
    remoteConfigDecoder paymentTypeDecoder >> remoteConfigPaymentTypes



-- INTERNAL


port remoteConfigTariffZones : (Decode.Value -> msg) -> Sub msg


port remoteConfigFareProducts : (Decode.Value -> msg) -> Sub msg


port remoteConfigUserProfiles : (Decode.Value -> msg) -> Sub msg


port remoteConfigConsents : (Decode.Value -> msg) -> Sub msg


port remoteConfigPaymentTypes : (Decode.Value -> msg) -> Sub msg


remoteConfigDecoder : Decoder a -> (Result () (List a) -> msg) -> (Decode.Value -> msg)
remoteConfigDecoder decoder msg =
    Decode.decodeValue (Decode.list decoder)
        >> Result.mapError (\_ -> ())
        >> msg
