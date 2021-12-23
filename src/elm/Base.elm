module Base exposing (AppInfo, OrgId(..))


type OrgId
    = AtB
    | NFK


type alias AppInfo =
    { version : String
    , commit : String
    , pageName : String
    , siteTitle : String
    , orgId : OrgId
    , zoneMapUrl : String
    , privacyDeclarationUrl : String
    , englishTranslationsUrl : Maybe String
    , newWebshopUrl : Maybe String
    , travelCardValidPrefix : String
    , defaultTravelModes : String
    }
