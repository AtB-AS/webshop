module Base exposing (AppInfo, OrgId(..))


type OrgId
    = AtB
    | NFK


type alias AppInfo =
    { title : String
    , logoUrl : String
    , orgId : OrgId
    , version : String
    , commit : String
    }
