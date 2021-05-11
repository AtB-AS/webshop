module Util.FareContract exposing (filterValidNow)

import Data.FareContract exposing (FareContract, FareContractState(..))
import Time


filterValidNow : Time.Posix -> List FareContract -> List FareContract
filterValidNow now fareContracts =
    fareContracts
        |> List.filter (\{ validTo } -> isValid validTo now)
        |> List.filter (.state >> (==) FareContractStateActivated)


isValid : Int -> Time.Posix -> Bool
isValid to posixNow =
    to >= Time.posixToMillis posixNow
