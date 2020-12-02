port module Service.Misc exposing (receiveTokens)

import Json.Encode as Encode


port receiveTokens : (Encode.Value -> msg) -> Sub msg
