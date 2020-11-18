module Service.Webshop exposing (hello)

import Environment exposing (Environment)
import Http


{-| Say hello. Fails if not logged in.
-}
hello : Environment -> Http.Request ()
hello env =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Atb-Install-Id" env.installId
            , Http.header "Authorization" ("Bearer " ++ env.token)
            ]
        , url = "/api/hellosecure"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
