module Data.RefData exposing
    ( FareProduct
    , LangString(..)
    , Limitation
    , TariffZone
    , UserProfile
    , UserType(..)
    )


type LangString
    = LangString String String


type alias TariffZone =
    { id : String
    , name : LangString
    }


type alias FareProduct =
    { id : String
    , name : LangString
    , description : LangString
    , alternativeNames : List LangString
    , limitations : List String
    }


type alias Limitation =
    { productId : String
    , limitations : List UserType
    }


type UserType
    = UserTypeAdult
    | UserTypeChild
    | UserTypeInfant
    | UserTypeSenior
    | UserTypeStudent
    | UserTypeYoungPerson
    | UserTypeSchoolPupil
    | UserTypeMilitary
    | UserTypeDisabled
    | UserTypeDisabledCompanion
    | UserTypeJobSeeker
    | UserTypeEmployee
    | UserTypeAnimal
    | UserTypeAnyone


type alias UserProfile =
    { id : String
    , name : LangString
    , description : LangString
    , alternativeNames : List LangString
    , age : ( Int, Int )
    , userType : UserType
    }
