module Util.PhoneNumber exposing (format, withCountryCode, withoutCountryCode)

import Util.NumberFormater as NF


withoutCountryCode : String -> String
withoutCountryCode phone =
    if String.startsWith "+" phone then
        -- NOTE: This isn't ideal permanently. If supporting multiple country codes, we should
        -- check for complete list as some are 3 digits.
        String.dropLeft 3 phone

    else
        phone


withCountryCode : String -> String
withCountryCode phone =
    if String.startsWith "+" phone then
        phone

    else
        "+47" ++ phone


format : String -> String
format phone =
    let
        countryCode =
            String.left 3 phone

        actualPhone =
            withoutCountryCode phone
    in
        NF.formatString
            [ NF.Str countryCode
            , NF.Space
            , NF.Digits 2
            , NF.Space
            , NF.Digits 2
            , NF.Space
            , NF.Digits 2
            , NF.Space
            , NF.Digits 2
            ]
            actualPhone
