module Service.RefData exposing (getFareProducts, getTariffZones, getUserProfiles)

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
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/tariff-zones") (Decode.list tariffZoneDecoder)


{-| Get list of fare products.
-}
getFareProducts : Environment -> Http.Request (List FareProduct)
getFareProducts env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/preassigned-fare-products") (Decode.list fareProductDecoder)


{-| Get list of user profiles.
-}
getUserProfiles : Environment -> Http.Request (List UserProfile)
getUserProfiles env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/user-profiles") (Decode.list userProfileDecoder)



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
        |> DecodeP.required "description" langStringDecoder
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)


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
