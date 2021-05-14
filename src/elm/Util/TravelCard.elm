module Util.TravelCard exposing (extractDigits, format)

import Util.NumberFormater as NF



{- Utils for mapping and handling TravelCard Logic.

   TODO Find better placement for these types of reusable domain logic modules.

-}


{-| Extract correct digits from valid travel card number
-}
extractDigits : String -> String
extractDigits =
    String.right 10 >> String.left 9


format : String -> String
format =
    NF.formatString [ NF.Digits 4, NF.Space, NF.Digits 4, NF.Space, NF.Digits 8 ]
