module Error exposing (Error, defaultError)

{-| Contains a general type for errors
-}


{-| Describes an error
-}
type alias Error =
    { title : String
    , message : String
    , code : Maybe Int
    }


{-| `Error` with default values
-}
defaultError : Error
defaultError =
    { title = "Unknown error"
    , message = "An unknown error has occured."
    , code = Nothing
    }
