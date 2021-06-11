module Util.TravelCard exposing (format, formatAnonymized, formatSignificant)

import Util.NumberFormater as NF



{- Utils for mapping and handling TravelCard Logic.

   TODO Find better placement for these types of reusable domain logic modules.

-}


format : String -> String
format =
    NF.formatString [ NF.Digits 4, NF.Space, NF.Digits 4, NF.Space, NF.Digits 8 ]


formatAnonymized : String -> String
formatAnonymized =
    NF.formatString [ NF.Str "XXXX", NF.Space, NF.Str "XX", NF.Digits 2, NF.Space, NF.Digits 7, NF.Str "X" ]


formatSignificant : String -> String
formatSignificant =
    NF.formatString [ NF.Digits 2, NF.Space, NF.Digits 7 ]
