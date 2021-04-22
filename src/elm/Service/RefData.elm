port module Service.RefData exposing
    ( getFareProducts
    , getTariffZones
    , getUserProfiles
    , onFareProducts
    , onTariffZones
    , onUserProfiles
    )

import Data.RefData exposing (FareProduct, LangString(..), TariffZone, UserProfile, UserType(..))
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Util.Http as HttpUtil


{-| Get list of tariff zones.
-}
getTariffZones : Environment -> Http.Request (List TariffZone)
getTariffZones env =
    HttpUtil.get env (env.refDataUrl ++ "/reference-data/v1/ATB/tariff-zones") (Http.expectJson (Decode.list tariffZoneDecoder))


{-| Get list of fare products.
-}
getFareProducts : Environment -> Http.Request (List FareProduct)
getFareProducts env =
    HttpUtil.get env (env.refDataUrl ++ "/reference-data/v1/ATB/preassigned-fare-products") (Http.expectJson (Decode.list fareProductDecoder))


{-| Get list of user profiles.
-}
getUserProfiles : Environment -> Http.Request (List UserProfile)
getUserProfiles env =
    HttpUtil.get env (env.refDataUrl ++ "/reference-data/v1/ATB/user-profiles") (Http.expectJson (Decode.list userProfileDecoder))



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
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)
        |> DecodeP.required "limitations" limitationsDecoder


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


onTariffZones : (Result () (List TariffZone) -> msg) -> Sub msg
onTariffZones =
    remoteConfigDecoder tariffZoneDecoder >> remoteConfigTariffZones


onFareProducts : (Result () (List FareProduct) -> msg) -> Sub msg
onFareProducts =
    remoteConfigDecoder fareProductDecoder >> remoteConfigFareProducts


onUserProfiles : (Result () (List UserProfile) -> msg) -> Sub msg
onUserProfiles =
    remoteConfigDecoder userProfileDecoder >> remoteConfigUserProfiles



-- INTERNAL


port remoteConfigTariffZones : (Decode.Value -> msg) -> Sub msg


port remoteConfigFareProducts : (Decode.Value -> msg) -> Sub msg


port remoteConfigUserProfiles : (Decode.Value -> msg) -> Sub msg


remoteConfigDecoder : Decoder a -> (Result () (List a) -> msg) -> (Decode.Value -> msg)
remoteConfigDecoder decoder msg =
    Decode.decodeValue (Decode.list decoder)
        >> Result.mapError (\_ -> ())
        >> msg
