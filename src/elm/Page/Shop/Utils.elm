module Page.Shop.Utils exposing (CommonModel, ModelSummary, TravelDateTime(..), defaultDerivedData, findLimitations, langString, modelSummary, nameFromFareProduct, nameFromUserType, stringFromTravelDate, stringFromZone, stringFromtravelDateTime, userTypeAsIdString)

import Data.RefData exposing (FareProduct, LangString(..), Limitation, TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer)
import List.Extra
import Shared exposing (Shared)
import Time
import Util.Status exposing (Status(..))
import Util.Time as TimeUtil


type TravelDateTime
    = TravelNow
    | TravelFuture (Maybe ( String, Int ))


type alias CommonModel a =
    { a
        | offers : Status (List Offer)
        , users : List ( UserType, Int )
        , product : Maybe String
        , fromZone : Maybe String
        , toZone : Maybe String
        , timeZone : Time.Zone
        , travelDateTime : TravelDateTime
        , travelDateTimeEnd : TravelDateTime
    }


type alias ModelSummary =
    { users : List ( String, Int )
    , product : Maybe String
    , start : Maybe String
    , duration : Maybe String
    }


defaultDerivedData : Shared -> List FareProduct -> ( String, String )
defaultDerivedData shared products =
    let
        firstZone =
            shared.tariffZones
                |> List.sortWith
                    (\a b ->
                        case ( a.name, b.name ) of
                            ( LangString _ nameA, LangString _ nameB ) ->
                                compare nameA nameB
                    )
                |> List.head
                |> Maybe.map .id
                |> Maybe.withDefault ""

        defaultProduct =
            products
                |> List.head
                |> Maybe.map .id
                |> Maybe.withDefault ""
    in
        ( firstZone, defaultProduct )


modelSummary : ( String, String ) -> Shared -> CommonModel a -> ModelSummary
modelSummary ( defaultZone, defaultProduct ) shared model =
    let
        product =
            Maybe.withDefault defaultProduct model.product
    in
        { users =
            model.users
                |> List.map
                    (Tuple.mapFirst (\a -> a |> nameFromUserType shared.userProfiles |> Maybe.withDefault "-"))
        , product = nameFromFareProduct shared.fareProducts product
        , start = stringFromTravelDate model.travelDateTime model.timeZone
        , duration = nameFromFareProduct shared.fareProducts product
        }


stringFromTravelDate : TravelDateTime -> Time.Zone -> Maybe String
stringFromTravelDate travelDateTime timeZone =
    case travelDateTime of
        TravelNow ->
            Just "Kjøpstidspunkt"

        TravelFuture (Just ( time, _ )) ->
            TimeUtil.isoStringToFullHumanized timeZone time

        _ ->
            Nothing


nameFromUserType : List UserProfile -> UserType -> Maybe String
nameFromUserType profiles userType =
    profiles
        |> List.Extra.find (.userType >> (==) userType)
        |> Maybe.map (.name >> langString)


nameFromFareProduct : List FareProduct -> String -> Maybe String
nameFromFareProduct products productId =
    products
        |> List.Extra.find (.id >> (==) productId)
        |> Maybe.map (.name >> langString)


stringFromZone : List TariffZone -> String -> { a | toZone : Maybe String, fromZone : Maybe String } -> Maybe String
stringFromZone tariffZones defaultZone model =
    let
        findName zone =
            tariffZones
                |> List.Extra.find (.id >> (==) zone)
                |> Maybe.map (.name >> langString)
                |> Maybe.withDefault "-"

        fromZoneName =
            findName (Maybe.withDefault defaultZone model.fromZone)

        toZoneName =
            findName (Maybe.withDefault defaultZone model.toZone)
    in
        if model.fromZone == model.toZone then
            Just <| "Reise i 1 sone (" ++ fromZoneName ++ ")"

        else
            Just <| "Reise fra sone " ++ fromZoneName ++ " til sone " ++ toZoneName


langString : LangString -> String
langString (LangString _ value) =
    value


stringFromtravelDateTime : TravelDateTime -> Maybe String
stringFromtravelDateTime travelDateTime =
    case travelDateTime of
        TravelFuture (Just ( time, _ )) ->
            Just time

        _ ->
            Nothing


findLimitations : String -> List Limitation -> List UserType
findLimitations productId fareProducts =
    fareProducts
        |> List.Extra.find (.productId >> (==) productId)
        |> Maybe.map .limitations
        |> Maybe.withDefault []


userTypeAsIdString : UserType -> String
userTypeAsIdString userType =
    case userType of
        UserTypeAdult ->
            "UserTypeAdult"

        UserTypeChild ->
            "UserTypeChild"

        UserTypeInfant ->
            "UserTypeInfant"

        UserTypeSenior ->
            "UserTypeSenior"

        UserTypeStudent ->
            "UserTypeStudent"

        UserTypeYoungPerson ->
            "UserTypeYoungPerson"

        UserTypeSchoolPupil ->
            "UserTypeSchoolPupil"

        UserTypeMilitary ->
            "UserTypeMilitary"

        UserTypeDisabled ->
            "UserTypeDisabled"

        UserTypeDisabledCompanion ->
            "UserTypeDisabledCompanion"

        UserTypeJobSeeker ->
            "UserTypeJobSeeker"

        UserTypeEmployee ->
            "UserTypeEmployee"

        UserTypeAnimal ->
            "UserTypeAnimal"

        UserTypeAnyone ->
            "UserTypeAnyone"
