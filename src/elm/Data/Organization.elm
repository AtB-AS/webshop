module Data.Organization exposing (OrganizationConfiguration, orgConfDecoder)

import Base exposing (OrgId(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP


type alias OrganizationConfiguration =
    { orgId : OrgId
    , siteTitle : String
    , zoneMapUrl : String
    , privacyDeclarationUrl : String
    , englishTranslationsUrl : Maybe String
    , travelCardValidPrefix : String
    }


orgConfDecoder : Decoder OrganizationConfiguration
orgConfDecoder =
    Decode.succeed OrganizationConfiguration
        |> DecodeP.required "orgId" orgIdDecoder
        |> DecodeP.required "siteTitle" Decode.string
        |> DecodeP.required "zoneMapUrl" Decode.string
        |> DecodeP.required "privacyDeclarationUrl" Decode.string
        |> DecodeP.optional "englishTranslationsUrl" (Decode.map Just Decode.string) Nothing
        |> DecodeP.required "travelCardValidPrefix" Decode.string


orgIdFromString : String -> OrgId
orgIdFromString provider =
    case provider of
        "nfk" ->
            NFK

        _ ->
            AtB


orgIdDecoder : Decoder OrgId
orgIdDecoder =
    Decode.andThen (orgIdFromString >> Decode.succeed) Decode.string
