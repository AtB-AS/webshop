module Util.Http exposing (get, post, put, request)

import Environment exposing (Environment)
import Http exposing (Body, Request)
import Json.Decode exposing (Decoder)


request : Environment -> String -> String -> Body -> Decoder a -> Request a
request env method url body decoder =
    Http.request
        { method = method
        , headers =
            [ Http.header "Atb-Install-Id" env.installId
            , Http.header "Authorization" ("Bearer " ++ env.token)
            ]
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


get : Environment -> String -> Decoder a -> Request a
get env url decoder =
    request env "GET" url Http.emptyBody decoder


post : Environment -> String -> Body -> Decoder a -> Request a
post env url body decoder =
    request env "POST" url body decoder


put : Environment -> String -> Body -> Decoder a -> Request a
put env url body decoder =
    request env "PUT" url body decoder
