module Util.Http exposing (delete, get, patch, post, put, request)

import Environment exposing (Environment)
import Http exposing (Body, Request)


request : Environment -> String -> String -> Body -> Http.Expect a -> Request a
request env method url body expect =
    Http.request
        { method = method
        , headers =
            [ Http.header "Atb-Distribution-Channel" "Web"
            , Http.header "Atb-Install-Id" env.installId
            , Http.header "Authorization" ("Bearer " ++ env.token)
            ]
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


get : Environment -> String -> Http.Expect a -> Request a
get env url expect =
    request env "GET" url Http.emptyBody expect


post : Environment -> String -> Body -> Http.Expect a -> Request a
post env url body expect =
    request env "POST" url body expect


patch : Environment -> String -> Body -> Http.Expect a -> Request a
patch env url body expect =
    request env "PATCH" url body expect


put : Environment -> String -> Body -> Http.Expect a -> Request a
put env url body expect =
    request env "PUT" url body expect


delete : Environment -> String -> Body -> Http.Expect a -> Request a
delete env url body expect =
    request env "DELETE" url body expect
