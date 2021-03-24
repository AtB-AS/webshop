port module Service.Phone exposing
    ( onError
    , onRequestCode
    , phoneConfirm
    , phoneLogin
    )

{-| A set of ports for dealing with phone login to Firebase Auth.
-}

import Json.Decode exposing (Value)


{-| Initiate login with the given phone number.
-}
port phoneLogin : String -> Cmd msg


{-| Confirm the login with the given code.
-}
port phoneConfirm : String -> Cmd msg


{-| Called when SMS has been sent and we need the user to enter the code.
-}
onRequestCode : msg -> Sub msg
onRequestCode msg =
    phoneRequestCode (\_ -> msg)


{-| Called on error during phone login.
-}
onError : (String -> msg) -> Sub msg
onError =
    phoneError



--INTERNAL


port phoneRequestCode : (Value -> msg) -> Sub msg


port phoneError : (String -> msg) -> Sub msg
