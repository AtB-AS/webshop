module Util.List exposing (find)


find : (a -> Bool) -> List a -> Maybe a
find f list =
    case list of
        [] ->
            Nothing

        hd :: tl ->
            if f hd then
                Just hd

            else
                find f tl
