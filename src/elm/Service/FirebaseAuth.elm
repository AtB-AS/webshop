port module Service.FirebaseAuth exposing
    ( signIn
    , signInError
    , signInInfo
    , signOut
    )

import Json.Encode


port signIn : () -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOut : () -> Cmd msg
