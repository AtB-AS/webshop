module Data.Organization exposing (OrganizationConfiguration, orgConfDecoder)

import Base exposing (OrgId(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP


type alias OrganizationConfiguration =
    { orgId : OrgId
    , logoUrl : String
    , siteTitle : String
    }


orgConfDecoder : Decoder OrganizationConfiguration
orgConfDecoder =
    Decode.succeed OrganizationConfiguration
        |> DecodeP.required "orgId" orgIdDecoder
        |> DecodeP.required "logoUrl" Decode.string
        |> DecodeP.required "siteTitle" Decode.string


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
