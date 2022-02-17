module Service.Ticket exposing
    ( endRecurringPayment
    , getPaymentStatus
    , getRecurringPayments
    , receipt
    , reserve
    , search
    )

import Data.PaymentType as PaymentType exposing (PaymentCard(..), PaymentSelection(..), PaymentType(..))
import Data.RefData exposing (UserType(..))
import Data.Ticket exposing (Offer, PaymentStatus, Price, RecurringPayment, Reservation)
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode exposing (Value)
import Url.Builder
import Util.Http as HttpUtil
import Util.Maybe as MaybeUtil
import Util.PhoneNumber


{-| Search for offers.
-}
search : Environment -> Maybe String -> String -> List ( UserType, Int ) -> List String -> Http.Request (List Offer)
search env travelDate product travellers zones =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl [ "ticket", "v1", "search", "zones" ] []

        body =
            [ ( "products", Encode.list Encode.string [ product ] )
            , ( "travellers", Encode.list encodeTraveller travellers )
            , ( "zones", Encode.list Encode.string zones )
            ]
                ++ (case travelDate of
                        Just dateTime ->
                            [ ( "travel_date", Encode.string dateTime ) ]

                        Nothing ->
                            []
                   )
    in
        HttpUtil.post env url (Http.jsonBody <| Encode.object body) (Http.expectJson (Decode.list offerDecoder))


{-| Reserve offers.
-}
reserve : Environment -> Maybe String -> PaymentSelection -> Bool -> List ( String, Int ) -> Http.Request Reservation
reserve env phoneNumber paymentSelection storePayment offers =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl
                [ "ticket", "v2", "reserve" ]
                []

        vippsPhoneNumber =
            Util.PhoneNumber.forVipps phoneNumber

        body =
            [ ( "offers", Encode.list encodeOffer offers )
            , encodePaymentSelection paymentSelection
            , ( "store_payment", encodeStorePayment paymentSelection storePayment )
            , ( "sca_excemption", Encode.bool (isScaExcemption paymentSelection) )
            , ( "payment_redirect_url", Encode.string (env.localUrl ++ "/payment?transaction_id={transaction_id}&payment_id={payment_id}&order_id={order_id}") )
            ]
                ++ MaybeUtil.mapWithDefault (\p -> [ ( "phone_number", Encode.string p ) ]) [] vippsPhoneNumber
    in
        HttpUtil.post env url (Http.jsonBody <| Encode.object body) (Http.expectJson reservationDecoder)


{-| Get a receipt for an order.
-}
receipt : Environment -> String -> String -> Http.Request ()
receipt env emailAddress orderId =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl
                [ "ticket", "v1", "receipt" ]
                []

        body =
            Encode.object
                [ ( "email_address", Encode.string emailAddress )
                , ( "order_id", Encode.string orderId )
                , ( "order_version", Encode.int 1 )
                ]
    in
        HttpUtil.post env url (Http.jsonBody body) (Http.expectStringResponse (\_ -> Ok ()))


getPaymentStatus : Environment -> Int -> Http.Request PaymentStatus
getPaymentStatus env paymentId =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl
                [ "ticket", "v1", "payments", String.fromInt paymentId ]
                []
    in
        HttpUtil.get env url (Http.expectJson paymentStatusDecoder)


getRecurringPayments : Environment -> Http.Request (List RecurringPayment)
getRecurringPayments env =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl
                [ "ticket", "v2", "recurring-payments" ]
                []
    in
        HttpUtil.get env url (Http.expectJson (Decode.list recurringPaymentDecoder))


endRecurringPayment : Environment -> Int -> Http.Request ()
endRecurringPayment env id =
    let
        url =
            Url.Builder.crossOrigin env.baseUrl
                [ "ticket", "v2", "recurring-payments", String.fromInt id ]
                []
    in
        HttpUtil.delete env url Http.emptyBody (Http.expectStringResponse (\_ -> Ok ()))



-- INTERNAL


paymentStatusDecoder : Decoder PaymentStatus
paymentStatusDecoder =
    Decode.succeed PaymentStatus
        |> DecodeP.required "order_id" Decode.string
        |> DecodeP.required "status" Decode.string
        |> DecodeP.required "payment_type" Decode.string


encodeTraveller : ( UserType, Int ) -> Value
encodeTraveller ( userType, count ) =
    let
        userTypeStr =
            case userType of
                UserTypeAdult ->
                    "ADULT"

                UserTypeChild ->
                    "CHILD"

                UserTypeInfant ->
                    "INFANT"

                UserTypeSenior ->
                    "SENIOR"

                UserTypeStudent ->
                    "STUDENT"

                UserTypeYoungPerson ->
                    "YOUTH"

                UserTypeSchoolPupil ->
                    -- No mapping
                    "STUDENT"

                UserTypeMilitary ->
                    "MILITARY"

                UserTypeDisabled ->
                    -- No mapping
                    "ADULT"

                UserTypeDisabledCompanion ->
                    -- Not sure?
                    "GUIDE_DOG"

                UserTypeJobSeeker ->
                    -- No mapping
                    "ADULT"

                UserTypeEmployee ->
                    -- No mapping
                    "ADULT"

                UserTypeAnimal ->
                    "ANIMAL"

                UserTypeAnyone ->
                    "ANYONE"
    in
        Encode.object
            [ ( "count", Encode.int count )
            , ( "id", Encode.string userTypeStr )
            , ( "user_type", Encode.string userTypeStr )
            ]


priceDecoder : Decoder Price
priceDecoder =
    Decode.succeed Price
        |> DecodeP.required "amount" Decode.string
        |> DecodeP.required "amount_float" Decode.float
        |> DecodeP.required "currency" Decode.string


offerDecoder : Decoder Offer
offerDecoder =
    Decode.succeed Offer
        |> DecodeP.required "offer_id" Decode.string
        |> DecodeP.required "traveller_id" decodeUserType
        |> DecodeP.required "prices" (Decode.list priceDecoder)
        |> DecodeP.required "traveller_id" Decode.string
        |> DecodeP.required "valid_to" Decode.string
        |> DecodeP.required "valid_from" Decode.string


decodeUserType : Decoder UserType
decodeUserType =
    Decode.andThen
        (\value ->
            case value of
                "ADULT" ->
                    Decode.succeed UserTypeAdult

                "CHILD" ->
                    Decode.succeed UserTypeChild

                "INFANT" ->
                    Decode.succeed UserTypeInfant

                "SENIOR" ->
                    Decode.succeed UserTypeSenior

                "STUDENT" ->
                    Decode.succeed UserTypeStudent

                "YOUTH" ->
                    Decode.succeed UserTypeYoungPerson

                "MILITARY" ->
                    Decode.succeed UserTypeMilitary

                "GUIDE_DOG" ->
                    Decode.succeed UserTypeDisabledCompanion

                "ANIMAL" ->
                    Decode.succeed UserTypeAnimal

                "ANYONE" ->
                    Decode.succeed UserTypeAnyone

                _ ->
                    Decode.fail "Invalid user type"
        )
        Decode.string


encodeOffer : ( String, Int ) -> Value
encodeOffer ( offerId, count ) =
    Encode.object
        [ ( "offer_id", Encode.string offerId )
        , ( "count", Encode.int count )
        ]


reservationDecoder : Decoder Reservation
reservationDecoder =
    Decode.succeed Reservation
        |> DecodeP.required "order_id" Decode.string
        |> DecodeP.required "payment_id" Decode.int
        |> DecodeP.required "transaction_id" Decode.int
        |> DecodeP.required "url" Decode.string


encodePaymentType : PaymentType -> Value
encodePaymentType =
    PaymentType.toInt >> Encode.int


decodePaymentType : Decoder PaymentType
decodePaymentType =
    Decode.int
        |> Decode.map PaymentType.fromInt
        |> Decode.andThen
            (\maybePaymentType ->
                case maybePaymentType of
                    Just paymentType ->
                        Decode.succeed paymentType

                    Nothing ->
                        Decode.fail "Invalid payment type"
            )


encodePaymentSelection : PaymentSelection -> ( String, Value )
encodePaymentSelection paymentSelection =
    case paymentSelection of
        NonRecurring paymentType ->
            ( "payment_type", encodePaymentType paymentType )

        Recurring id ->
            ( "recurring_payment_id", Encode.int id )


isScaExcemption : PaymentSelection -> Bool
isScaExcemption paymentSelection =
    case paymentSelection of
        NonRecurring PaymentType.Vipps ->
            False

        _ ->
            True


recurringPaymentDecoder : Decoder RecurringPayment
recurringPaymentDecoder =
    Decode.succeed RecurringPayment
        |> DecodeP.required "id" Decode.int
        |> DecodeP.required "payment_type" decodePaymentType
        |> DecodeP.required "masked_pan" Decode.string
        |> DecodeP.required "expires_at" Decode.string


encodeStorePayment : PaymentSelection -> Bool -> Value
encodeStorePayment paymentSelection storePayment =
    case paymentSelection of
        NonRecurring (Nets _) ->
            Encode.bool storePayment

        _ ->
            Encode.bool False
