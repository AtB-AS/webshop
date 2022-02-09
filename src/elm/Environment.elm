module Environment exposing
    ( DistributionEnvironment(..)
    , Environment
    , Language(..)
    )


type Language
    = English
    | Norwegian


{-| Describes the distribution environment the app is running in.
-}
type DistributionEnvironment
    = Production
    | Development


type alias Environment =
    { distributionEnv : DistributionEnvironment
    , localUrl : String
    , baseUrl : String
    , language : Language
    , installId : String
    , showValidityWarning : Bool
    , intercomEnabled : Bool
    , customerId : Maybe String
    , customerNumber : Maybe Int
    , customerEmail : String
    , token : String
    }
