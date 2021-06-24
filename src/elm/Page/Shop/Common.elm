module Page.Shop.Common exposing (CommonModel, TravelDateTime(..), findLimitations, modelSummary, nameFromFareProduct, nameFromUserType, stringFromZone, viewSummary, viewUserProfiles, viewZones)

import Data.RefData exposing (FareProduct, LangString(..), Limitation, TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import List.Extra
import Page.Shop.Summary as SummaryPage
import Shared exposing (Shared)
import Time
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Input.Radio as Radio
import Ui.Input.Select as Select
import Ui.LabelItem
import Ui.LoadingText
import Ui.Message as Message
import Ui.Section as Section
import Util.Format
import Util.Func as Func
import Util.Status exposing (Status(..))
import Util.Time as TimeUtil


type TravelDateTime
    = TravelNow
    | TravelFuture (Maybe String)


type alias CommonModel a =
    { a
        | offers : Status (List Offer)
        , users : List ( UserType, Int )
        , product : Maybe String
        , fromZone : Maybe String
        , toZone : Maybe String
        , timeZone : Time.Zone
        , travelDateTime : TravelDateTime
    }


viewUserProfiles : String -> CommonModel a -> (UserType -> Bool -> msg) -> Shared -> Html msg
viewUserProfiles defaultProduct model onUserSelect shared =
    let
        product =
            Maybe.withDefault defaultProduct model.product
    in
        shared.userProfiles
            |> List.filter (.userType >> Func.flip List.member (findLimitations product shared.productLimitations))
            |> List.filter (.userType >> (/=) UserTypeAnyone)
            |> List.map (viewUserProfile model onUserSelect)
            |> Radio.viewGroup "Reisende"


viewUserProfile : CommonModel a -> (UserType -> Bool -> msg) -> UserProfile -> Html msg
viewUserProfile model onUserSelect userProfile =
    let
        isCurrent =
            List.any (Tuple.first >> (==) userProfile.userType) model.users
    in
        Radio.init (userTypeAsIdString userProfile.userType)
            |> Radio.setTitle (langString userProfile.name)
            |> Radio.setName "userprofile"
            |> Radio.setSubtitle (Just <| langString userProfile.description)
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| onUserSelect userProfile.userType)
            |> Radio.view


viewSummary : Shared -> CommonModel a -> Bool -> msg -> Html msg
viewSummary shared model disableButtons onToSummaryClick =
    let
        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

                _ ->
                    False

        summary =
            case model.offers of
                Loaded offers ->
                    Just <|
                        SummaryPage.makeSummary
                            { productId = Maybe.withDefault "" model.product
                            , fromZoneId = Maybe.withDefault "" model.fromZone
                            , toZoneId = Maybe.withDefault "" model.toZone
                            , travelDate = stringFromtravelDateTime model.travelDateTime
                            }
                            offers
                            shared

                _ ->
                    Nothing
    in
        Section.init
            |> Section.viewWithOptions
                [ if emptyOffers then
                    Message.warning "Finner ingen tilgjengelige billetter."

                  else
                    Section.viewPaddedItem
                        [ Ui.LabelItem.viewHorizontal
                            "Total:"
                            [ H.p [ A.class "shop__summaryPrice" ]
                                [ summary
                                    |> Maybe.map .totalPrice
                                    |> Maybe.map (Func.flip Util.Format.float 2)
                                    |> Maybe.map H.text
                                    |> Maybe.withDefault (Ui.LoadingText.view "1.6875rem" "5rem")
                                , H.small [] [ H.text "kr" ]
                                ]
                            ]
                        , Ui.LabelItem.viewHorizontal "Hvorav mva:"
                            [ summary
                                |> Maybe.map .totalVat
                                |> Maybe.map (Func.flip Util.Format.float 2)
                                |> Maybe.map (Func.flip (++) " kr")
                                |> Maybe.map H.text
                                |> Maybe.withDefault (Ui.LoadingText.view "1rem" "3rem")
                            ]
                        ]
                , B.init "Gå til oppsummering"
                    |> B.setDisabled disableButtons
                    |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                    |> B.setOnClick (Just onToSummaryClick)
                    |> B.primary Primary_2
                ]


viewZones : CommonModel a -> String -> List TariffZone -> (String -> msg) -> (String -> msg) -> Html msg
viewZones model defaultZone zones onFromZoneSelected onToZoneSelected =
    let
        sortedZones =
            List.sortWith
                (\a b ->
                    case ( a.name, b.name ) of
                        ( LangString _ nameA, LangString _ nameB ) ->
                            compare nameA nameB
                )
                zones

        selectedFromZone =
            Maybe.withDefault defaultZone model.fromZone

        selectedToZone =
            Maybe.withDefault defaultZone model.toZone
    in
        Section.viewItem
            [ Section.viewHorizontalGroup
                [ Select.init "travelFromZone"
                    |> Select.setTitle (Just "Avreisesone")
                    |> Select.setOnInput (Just onFromZoneSelected)
                    |> Select.view (List.map (viewZone selectedFromZone) sortedZones)
                , Select.init "travelToZone"
                    |> Select.setTitle (Just "Ankomstsone")
                    |> Select.setOnInput (Just onToZoneSelected)
                    |> Select.view (List.map (viewZone selectedToZone) sortedZones)
                ]
            , Section.viewPaddedItem [ H.p [] [ H.a [ A.href "https://atb.no/soner", A.target "_blank" ] [ H.text "Se sonekart og beskrivelser (åpner ny side)" ] ] ]
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| langString zone.name ]


type alias ModelSummary =
    { users : List ( String, Int )
    , product : Maybe String
    , start : Maybe String
    , zones : Maybe String
    , duration : Maybe String
    }


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
        , start = stringFromStart model
        , zones = stringFromZone shared.tariffZones defaultZone model
        , duration = nameFromFareProduct shared.fareProducts product
        }


stringFromStart : CommonModel a -> Maybe String
stringFromStart model =
    case model.travelDateTime of
        TravelNow ->
            Just "Kjøpstidspunkt"

        TravelFuture (Just time) ->
            TimeUtil.isoStringToFullHumanized model.timeZone time

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


stringFromZone : List TariffZone -> String -> CommonModel a -> Maybe String
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
        TravelFuture (Just time) ->
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
