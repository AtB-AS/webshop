port module Service.FirebaseAuth exposing
    ( AuthError
    , Provider(..)
    , authError
    , confirmPhone
    , loginEmail
    , loginPhone
    , onAuthEmailUpdate
    , onError
    , onPasswordReset
    , onRequestCode
    , phoneRequestCode
    , providerDecoder
    , providerFromString
    , registerEmail
    , resetPassword
    , resetPasswordDone
    , signInError
    , signOut
    , signedInInfo
    , updateAuthEmail
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode


type Provider
    = Google
    | Microsoft
    | Phone
    | Password
    | Anonymous
    | Unknown String


type alias AuthError =
    { code : String
    , message : String
    }



-- PORTS


port signedInInfo : (Encode.Value -> msg) -> Sub msg


port signInError : (Encode.Value -> msg) -> Sub msg


port signOutHandler : () -> Cmd msg


{-| Initiate login with the given email/password.
-}
port loginEmail : { email : String, password : String } -> Cmd msg


{-| Initiate register with the given email/password.
Will login after successful register.
-}
port registerEmail : { email : String, password : String } -> Cmd msg


{-| Initiate login with the given phone number.
-}
port loginPhone : String -> Cmd msg


{-| Confirm the login with the given code.
-}
port confirmPhone : String -> Cmd msg


{-| Confirmation code from phone authentication recieved.
-}
port phoneRequestCode : (Value -> msg) -> Sub msg


{-| Error recieved from authentication module in index.js.
-}
port authError : (String -> msg) -> Sub msg


{-| Resetting password is done
-}
port resetPasswordDone : (Value -> msg) -> Sub msg


{-| Reset password by passing in email
-}
port resetPassword : String -> Cmd msg


port updateAuthEmail : String -> Cmd msg


port updateAuthEmailDone : (Encode.Value -> msg) -> Sub msg



-- HELPERS


signOut : Cmd msg
signOut =
    signOutHandler ()


{-| Called when SMS has been sent and we need the user to enter the code.
-}
onRequestCode : msg -> Sub msg
onRequestCode msg =
    phoneRequestCode (\_ -> msg)


{-| Called on error during phone login.
-}
onError : (String -> msg) -> Sub msg
onError =
    authError


{-| Called on password reset
-}
onPasswordReset : (Value -> msg) -> Sub msg
onPasswordReset =
    resetPasswordDone


{-| Called on update login email
-}
onAuthEmailUpdate : (Maybe AuthError -> msg) -> Sub msg
onAuthEmailUpdate msg =
    updateAuthEmailDone
        (Decode.decodeValue authErrorDecoder
            >> Result.toMaybe
            >> msg
        )



-- PROVIDER TO/FROM


authErrorDecoder : Decoder AuthError
authErrorDecoder =
    Decode.succeed AuthError
        |> DecodeP.required "code" Decode.string
        |> DecodeP.required "message" Decode.string


{-| Parse a string into a provider.
-}
providerFromString : String -> Provider
providerFromString provider =
    case provider of
        "google.com" ->
            Google

        "microsoft.com" ->
            Microsoft

        "phone" ->
            Phone

        "anonymous" ->
            Anonymous

        value ->
            Unknown value


{-| Decode a provider.
-}
providerDecoder : Decoder Provider
providerDecoder =
    Decode.andThen (providerFromString >> Decode.succeed) Decode.string
