module Util.FareContract exposing (filterValidAtTime, hasValidState, isValid)

import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Time


filterValidAtTime : Time.Posix -> List FareContract -> List FareContract
filterValidAtTime timePosix fareContracts =
    fareContracts
        |> List.filter (\fc -> isValid timePosix fc.validFrom fc.validTo)
        |> List.filter (hasValidTravelRight timePosix)
        |> List.filter hasValidState


isValid : Time.Posix -> Int -> Int -> Bool
isValid timePosix from to =
    let
        millis =
            Time.posixToMillis timePosix
    in
        from <= millis && to >= millis



-- INTERNAL


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
                                List.any (\tr -> isValid now tr.startDateTime tr.endDateTime) carnetType.usedAccesses
                        in
                            hasTicketsLeft || hasActiveAccess

                    _ ->
                        True
            )


{-| Check if a FareContract has valid state. If it is refunded or
cancelled it is not valid.
-}
hasValidState : FareContract -> Bool
hasValidState contract =
    contract.state == FareContractStateNotActivated || contract.state == FareContractStateActivated
