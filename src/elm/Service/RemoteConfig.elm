port module Service.RemoteConfig exposing (init, onRemoteConfig)

import Data.RefData exposing (FareProduct, LangString(..), TariffZone, UserProfile, UserType(..))
import Data.RemoteConfig exposing (RemoteConfig)
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Util.Http as HttpUtil


init : RemoteConfig
init =
    { vat_percent = 6
    }


onRemoteConfig : (Result () RemoteConfig -> msg) -> Sub msg
onRemoteConfig =
    remoteConfigDecoder remoteConfigDataDecoder >> remoteConfigData



-- INTERNAL


port remoteConfigData : (Decode.Value -> msg) -> Sub msg


remoteConfigDataDecoder : Decoder RemoteConfig
remoteConfigDataDecoder =
    Decode.succeed RemoteConfig
        |> DecodeP.required "vat_percent" Decode.int


remoteConfigDecoder : Decoder a -> (Result () a -> msg) -> (Decode.Value -> msg)
remoteConfigDecoder decoder msg =
    Decode.decodeValue decoder
        >> Result.mapError (\_ -> ())
        >> msg
