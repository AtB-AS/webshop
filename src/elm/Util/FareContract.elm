module Util.FareContract exposing (filterValidNow, isValid)

import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Time


filterValidNow : Time.Posix -> List FareContract -> List FareContract
filterValidNow now fareContracts =
    fareContracts
        |> List.filter (.validTo >> isValid now)
        |> List.filter (hasValidTravelRight now)


{-| Check if a FareContract has valid travelRights.
Currently explicitly checking for the carnet type and if
-}
hasValidTravelRight : Time.Posix -> FareContract -> Bool
hasValidTravelRight now contract =
    contract.travelRights
        |> List.any
            (\travelRight ->
                case travelRight of
                    CarnetTicket carnetType ->
                        let
                            hasTicketsLeft =
                                carnetType.numberOfUsedAccesses < carnetType.maximumNumberOfAccesses

                            hasActiveAccess =
                                List.any (.endDateTime >> .timestamp >> isValid now) carnetType.usedAccesses
                        in
                            hasTicketsLeft || hasActiveAccess

                    _ ->
                        True
            )


isValid : Time.Posix -> Int -> Bool
isValid posixNow to =
    to >= Time.posixToMillis posixNow
