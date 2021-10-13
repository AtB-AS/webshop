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

    -- Assets should be handled in design-system at a later stage
    , logoUrl : String
    }
