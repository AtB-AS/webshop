port module Service.RemoteConfig exposing (onVatPercent)


onVatPercent : (Float -> msg) -> Sub msg
onVatPercent =
    remoteConfigVatPercent



-- INTERNAL


port remoteConfigVatPercent : (Float -> msg) -> Sub msg
