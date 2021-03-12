port module Service.Misc exposing
    ( Profile
    , convertTime
    , convertedTime
    , onProfileChange
    , onboardingDone
    , onboardingStart
    , openWindow
    , receiveTokens
    , saveProfile
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode


port openWindow : String -> Cmd msg


port receiveTokens : (Encode.Value -> msg) -> Sub msg


port convertTime : ( String, String ) -> Cmd msg


port convertedTime : (String -> msg) -> Sub msg


port onboardingStart : (String -> msg) -> Sub msg


port onboardingDone : () -> Cmd msg


type alias Profile =
    { id : String
    , firstName : String
    , lastName : String
    , phone : String
    , email : String
    , travelCard : Maybe String
    }


port firestoreReadProfile : (Encode.Value -> msg) -> Sub msg


port firestoreWriteProfile : Encode.Value -> Cmd msg


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "firstName" Decode.string
        |> DecodeP.required "surname" Decode.string
        |> DecodeP.required "phone" Decode.string
        |> DecodeP.required "email" Decode.string
        |> DecodeP.optional "travelcard" (Decode.map Just Decode.string) Nothing


encodeProfile : Profile -> Encode.Value
encodeProfile profile =
    Encode.object
        [ ( "id", Encode.string profile.id )
        , ( "firstName", Encode.string profile.firstName )
        , ( "surname", Encode.string profile.lastName )
        , ( "phone", Encode.string profile.phone )
        , ( "email", Encode.string profile.email )
        , ( "travelcard"
          , profile.travelCard
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        ]


{-| Whenever the profile has been changed, this event will be fired.
-}
onProfileChange : (Maybe Profile -> msg) -> Sub msg
onProfileChange msg =
    firestoreReadProfile
        (Decode.decodeValue profileDecoder
            >> Result.toMaybe
            >> msg
        )


{-| Save the profile. Note that all fields must be correct.
-}
saveProfile : Profile -> Cmd msg
saveProfile =
    encodeProfile >> firestoreWriteProfile
