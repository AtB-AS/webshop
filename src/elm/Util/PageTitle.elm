module Util.PageTitle exposing (pageTitle)

import Maybe
import Util.Func


pageTitle : String -> Maybe String -> String
pageTitle siteTitle subTitle =
    (subTitle
        |> Maybe.map (Util.Func.flip (++) " - ")
        |> Maybe.withDefault ""
    )
        ++ siteTitle
