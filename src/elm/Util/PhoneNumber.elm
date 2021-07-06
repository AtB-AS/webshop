module Util.PhoneNumber exposing (format, isDefaultCountryCode, withDefaultCountryCode, withoutCountryCode)

import Util.NumberFormater as NF


withoutCountryCode : String -> String
withoutCountryCode phone =
    if String.startsWith "+" phone then
        -- NOTE: This isn't ideal permanently. If supporting multiple country codes, we should
        -- check for complete list as some are 3 digits.
        String.dropLeft 3 phone

    else
        phone


withDefaultCountryCode : String -> String
withDefaultCountryCode phone =
    if String.isEmpty phone then
        phone

    else if String.startsWith "+" phone then
        phone

    else
        "+47" ++ phone


isDefaultCountryCode : String -> Bool
isDefaultCountryCode =
    String.startsWith "+47"


format : String -> String
format phone =
    let
        countryCode =
            String.left 3 phone

        actualPhone =
            withoutCountryCode phone
    in
        if not (isDefaultCountryCode phone) then
            phone

        else
            -- @TODO This will be incorrect on country codes other than 2 digits.
            -- Currently visual noice, but not critica. At some point extend to use
            -- proper parsing of country codes.
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
