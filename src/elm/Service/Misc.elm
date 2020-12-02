port module Service.Misc exposing (openWindow, receiveTokens)

import Json.Encode as Encode


port openWindow : String -> Cmd msg


port receiveTokens : (Encode.Value -> msg) -> Sub msg
