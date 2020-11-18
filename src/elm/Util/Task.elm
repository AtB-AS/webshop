module Util.Task exposing (doTask, maybeAttemptTask, maybePerformTask)

{-| Helper functions for dealing with `Task`s.
-}

import Task exposing (Task)


{-| Perform a task. This is often used to run a msg in some update function.
-}
doTask : msg -> Cmd msg
doTask =
    Task.succeed >> Task.perform identity


{-| Attempt a task if the `Maybe` contains a value.
-}
maybeAttemptTask : (Result err a -> msg) -> Maybe (Task err a) -> Cmd msg
maybeAttemptTask callback =
    Maybe.map (Task.attempt callback)
        >> Maybe.withDefault Cmd.none


{-| Perform a task if the `Maybe` contains a value.
-}
maybePerformTask : (a -> msg) -> Maybe (Task Never a) -> Cmd msg
maybePerformTask callback =
    Maybe.map (Task.perform callback)
        >> Maybe.withDefault Cmd.none
