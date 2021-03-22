port module Service.FirebaseAuth exposing
    ( Provider(..)
    , providerDecoder
    , providerFromString
    , providerToString
    , signIn
    , signInError
    , signInInfo
    , signOut
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode


type Provider
    = Google
    | Microsoft
    | Phone
    | Anonymous
    | Unknown String



-- PORTS


port signInHandler : String -> Cmd msg


port signInInfo : (Json.Encode.Value -> msg) -> Sub msg


port signInError : (Json.Encode.Value -> msg) -> Sub msg


port signOutHandler : () -> Cmd msg



-- HELPERS


signIn : Provider -> Cmd msg
signIn =
    providerToString >> signInHandler


signOut : Cmd msg
signOut =
    signOutHandler ()



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

        Unknown _ ->
            "unknown"
