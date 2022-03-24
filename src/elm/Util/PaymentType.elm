module Util.PaymentType exposing (format)

import Data.PaymentType as PaymentType exposing (PaymentType)
import Data.PaymentTypeGroup as PaymentTypeGroup exposing (PaymentTypeGroup)


format : List PaymentType -> List PaymentTypeGroup -> String
format paymentTypes paymentTypeGroups =
    case List.head paymentTypes of
        Just paymentType ->
            PaymentType.format paymentType

        Nothing ->
            case List.head paymentTypeGroups of
                Just paymentTypeGroup ->
                    PaymentTypeGroup.format paymentTypeGroup

                Nothing ->
                    "Ukjent"
