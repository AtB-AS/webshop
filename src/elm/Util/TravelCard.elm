module Util.TravelCard exposing (format, formatSignificant, serverErrorToString)

import Http exposing (Error(..))
import Util.Func
import Util.NumberFormater as NF



{- Utils for mapping and handling TravelCard Logic.

   TODO Find better placement for these types of reusable domain logic modules.

-}


format : String -> String
format =
    NF.formatString [ NF.Digits 4, NF.Space, NF.Digits 4, NF.Space, NF.Digits 8 ]


formatSignificant : String -> String
formatSignificant =
    NF.formatString [ NF.Digits 2, NF.Space, NF.Digits 7 ]


serverErrorToString : (String -> Result error String) -> Error -> String
serverErrorToString encode error =
    case error of
        BadStatus { status, body } ->
            case status.code of
                500 ->
                    "Det skjedde en feil med tjenesten. Prøv igjen senere."

                409 ->
                    "Dette t:kortet eksisterer ikke eller er allerede registrert."

                400 ->
                    case encode body of
                        Ok errorMessage ->
                            translateErrorString errorMessage

                        _ ->
                            "Innsendt informasjon ser ut til å ikke stemme. Prøv igjen er du snill."

                _ ->
                    "Unknown error"

        _ ->
            "Fikk ikke kontakt med tjenesten. Sjekk om du er på nett og prøv igjen."


translateErrorString : String -> String
translateErrorString error =
    let
        check =
            Util.Func.flip String.contains error
    in
        if check "id length" then
            "t:kortnummeret må bestå av 16 siffer"

        else if check "be numeric" then
            "t:kortnummeret kan kun bestå av tall"

        else if check "incorrect" then
            "Det kan se ut som du har tastet inn feil t:kortnummer, se over og kontroller at alt stemmer."

        else
            "Fikk ikke lagret t:kortnummeret ditt. Ta kontakt med kundeservice om problemet vedvarer."
