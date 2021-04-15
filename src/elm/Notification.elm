module Notification exposing
    ( Notification
    , decrementTimer
    , init
    , map
    , setContent
    , setTimer
    )

{-| Contains a general type and combinators for a notification.
-}

import Html as H exposing (Html)


{-| Describes a notification.

You should prefer to use `init` and builder functions
rather than constructing this manually.

-}
type alias Notification msg =
    { content : Html msg
    , timer : Maybe Int
    }


{-| Create a `Notification` with default values.

Using this function and provided builder functions should
be your preferred approach when creating `Notification` instances.

-}
init : Notification msg
init =
    { content = H.text ""
    , timer = Just 10
    }


{-| Set the content of the notification.
-}
setContent : Html msg -> Notification msg -> Notification msg
setContent content notification =
    { notification | content = content }


{-| Set the timer of a notification.
-}
setTimer : Int -> Notification msg -> Notification msg
setTimer timer confirm =
    { confirm | timer = Just timer }


{-| Set the timer of a notification.
-}
decrementTimer : Notification msg -> Notification msg
decrementTimer confirm =
    { confirm | timer = confirm.timer |> Maybe.map (flip (-) 1) }


{-| Maps a function over `Notification`
-}
map : (a -> msg) -> Notification a -> Notification msg
map f notification =
    { content = H.map f notification.content
    , timer = notification.timer
    }


flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a
