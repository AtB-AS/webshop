port module Service.FirebaseAuth exposing
    ( Provider(..)
    , authError
    , confirmPhone
    , loginEmail
    , loginPhone
    , onError
    , onRequestCode
    , phoneRequestCode
    , providerDecoder
    , providerFromString
    , providerToString
    , registerEmail
    , signInError
    , signOut
    , signedInInfo
    )

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode


type Provider
    = Google
    | Microsoft
    | Phone
    | Password
    | Anonymous
    | Unknown String



-- PORTS


port signedInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


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



-- PROVIDER TO/FROM


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


{-| Turn a provider into a string.
-}
providerToString : Provider -> String
providerToString provider =
    case provider of
        Google ->
            "google.com"

        Microsoft ->
            "microsoft.com"

        Phone ->
            "phone"

        Anonymous ->
            "anonymous"

        Password ->
            "password"

        Unknown _ ->
            "unknown"
