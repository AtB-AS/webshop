module Util.PageTitle exposing (pageTitle)

import Maybe
import Util.Func


pageTitle : Maybe String -> String
pageTitle subTitle =
    (subTitle
        |> Maybe.map (Util.Func.flip (++) " - ")
        |> Maybe.withDefault ""
    )
        ++ "AtB Nettbutikk"
