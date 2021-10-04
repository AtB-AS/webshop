module Base exposing (AppInfo, OrgId(..))


type OrgId
    = AtB
    | NFK


type alias AppInfo =
    { pageName : String
    , siteTitle : String
    , version : String
    , commit : String
    , logoUrl : String
    , orgId : OrgId
    , loginIllustrationUrl : String
    }
