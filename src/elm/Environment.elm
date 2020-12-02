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
    , baseUrl : String
    , language : Language
    , installId : String
    , customerId : Maybe String
    , customerNumber : Int
    , token : String
    }
