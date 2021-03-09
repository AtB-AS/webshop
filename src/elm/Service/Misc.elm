port module Service.Misc exposing
    ( convertTime
    , convertedTime
    , openWindow
    , receiveTokens
    )

import Json.Encode as Encode


port openWindow : String -> Cmd msg


port receiveTokens : (Encode.Value -> msg) -> Sub msg


port convertTime : ( String, String ) -> Cmd msg


port convertedTime : (String -> msg) -> Sub msg
