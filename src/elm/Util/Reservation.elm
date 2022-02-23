module Util.Reservation exposing (filterValidReservations, isNotAbortedReservation, isReservationWithinAnHour)

import Data.Reservation exposing (PaymentStatus(..), Reservation)
import Time


isNotAbortedReservation : Reservation -> Bool
isNotAbortedReservation reservation =
    case reservation.paymentStatus of
        Nothing ->
            False

        Just paymentStatus ->
            not <| List.member paymentStatus [ CANCEL, CREDIT, REJECT ]


isReservationWithinAnHour : Time.Posix -> Reservation -> Bool
isReservationWithinAnHour now reservation =
    let
        anHourAfterReservation =
            reservation.created + (60 * 60 * 1000)

        nowMs =
            Time.posixToMillis now
    in
        anHourAfterReservation >= nowMs


filterValidReservations : Time.Posix -> List Reservation -> List Reservation
filterValidReservations currentTime reservations =
    reservations
        |> List.filter (.paymentStatus >> (/=) Nothing)
        |> List.filter isNotAbortedReservation
        |> List.filter (isReservationWithinAnHour currentTime)
