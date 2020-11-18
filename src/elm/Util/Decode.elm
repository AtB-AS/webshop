module Util.Decode exposing (requiredIndex)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP


requiredIndex : Int -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredIndex index valDecoder decoder =
    DecodeP.custom (Decode.index index valDecoder) decoder
