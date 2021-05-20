module Util.PhoneNumber exposing (format)

import Util.NumberFormater as NF


format : String -> String
format phone =
    let
        countryCode =
            String.left 3 phone

        actualPhone =
            String.replace countryCode "" phone
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
