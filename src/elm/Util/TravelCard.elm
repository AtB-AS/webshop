module Util.TravelCard exposing (extractDigits)

{-| Utils for mapping and handling TravelCard Logic.

TODO Find better placement for these types of reusable domain logic modules.

-}


{-| Extract correct digits from valid travel card number
-}
extractDigits : String -> String
extractDigits =
    String.right 10 >> String.left 9
