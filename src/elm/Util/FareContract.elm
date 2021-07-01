module Util.FareContract exposing (filterValidNow, isValid)

import Data.FareContract exposing (FareContract, FareContractState(..))
import Time


filterValidNow : Time.Posix -> List FareContract -> List FareContract
filterValidNow now fareContracts =
    fareContracts
        |> List.filter (.validTo >> isValid now)
        |> List.filter (.state >> (==) FareContractStateActivated)


isValid : Time.Posix -> Int -> Bool
isValid posixNow to =
    to >= Time.posixToMillis posixNow
