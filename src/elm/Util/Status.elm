module Util.Status exposing (Status(..))


type Status a
    = NotLoaded
    | Loading (Maybe a)
    | Loaded a
    | Failed String
