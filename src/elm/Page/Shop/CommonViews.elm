module Page.Shop.CommonViews exposing (viewSummary, viewUserProfiles, viewZones)

import Base exposing (AppInfo)
import Data.RefData exposing (LangString(..), TariffZone, UserProfile, UserType(..))
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr
import Html.Extra
import Page.Shop.Summary as SummaryPage
import Page.Shop.Utils as Utils exposing (CommonModel)
import Shared exposing (Shared)
import Ui.AccessibleFieldset
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Input.Radio as Radio
import Ui.Input.Select as Select
import Ui.LabelItem
import Ui.LoadingText
import Ui.Message as Message
import Ui.Section as Section
import Util.Format
import Util.Func as Func
import Util.Maybe
import Util.Status exposing (Status(..))


viewUserProfiles : String -> CommonModel a -> (UserType -> msg) -> Shared -> Html msg
viewUserProfiles defaultProduct model onUserSelect shared =
    let
        product =
            Maybe.withDefault defaultProduct model.product
    in
        shared.userProfiles
            |> List.filter (.userType >> Func.flip List.member (Utils.findLimitations product shared.productLimitations))
            |> List.filter (.userType >> (/=) UserTypeAnyone)
            |> List.map (viewUserProfile model onUserSelect)
            |> Radio.viewGroup "Reisende"


viewUserProfile : CommonModel a -> (UserType -> msg) -> UserProfile -> Html msg
viewUserProfile model onUserSelect userProfile =
    let
        isCurrent =
            List.any (Tuple.first >> (==) userProfile.userType) model.users
    in
        Radio.init (Utils.userTypeAsIdString userProfile.userType)
            |> Radio.setTitle (Utils.langString userProfile.name)
            |> Radio.setName "userprofile"
            |> Radio.setSubtitle (Just <| Utils.langString userProfile.description)
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| onUserSelect userProfile.userType)
            |> Radio.view


viewSummary : Shared -> CommonModel a -> Bool -> msg -> Maybe (Html msg) -> Html msg
viewSummary shared model disableButtons onToSummaryClick maybeInfo =
    let
        error =
            case model.offers of
                Loaded [] ->
                    Just "Finner ingen tilgjengelige billetter."

                Failed errorMessage ->
                    Just errorMessage

                _ ->
                    Nothing

        noTravelCard =
            shared.profile
                |> Util.Maybe.flatMap .travelCard
                |> Util.Maybe.mapWithDefault (\_ -> False) True

        summary =
            case model.offers of
                Loaded offers ->
                    Just <|
                        SummaryPage.makeSummary
                            { productId = Maybe.withDefault "" model.product
                            , fromZoneId = Maybe.withDefault "" model.fromZone
                            , toZoneId = Maybe.withDefault "" model.toZone
                            , travelDate = model.travelDateTime
                            , travelDateEnd = model.travelDateTimeEnd
                            , timeZone = model.timeZone
                            }
                            offers
                            shared

                _ ->
                    Nothing
    in
        Section.init
            |> Section.viewWithOptions
                [ case error of
                    Just errorMessage ->
                        Message.warning errorMessage

                    _ ->
                        Section.viewPaddedItem
                            [ Ui.LabelItem.viewHorizontal
                                "Total:"
                                [ H.p
                                    [ A.class "shop__summaryPrice"
                                    , A.attribute "aria-live" "polite"
                                    ]
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
                , Html.Extra.viewIf noTravelCard (Message.info "Legg til et t:kort før kjøp av billett")
                , Html.Extra.viewMaybe identity maybeInfo
                , B.init "Gå til oppsummering"
                    |> B.setDisabled (disableButtons || noTravelCard)
                    |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                    |> B.setOnClick (Just onToSummaryClick)
                    |> B.primary Primary_2
                ]


viewZones : CommonModel a -> AppInfo -> String -> List TariffZone -> (String -> msg) -> (String -> msg) -> Bool -> Html msg
viewZones model appInfo defaultZone zones onFromZoneSelected onToZoneSelected disabled =
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
            [ Ui.AccessibleFieldset.view "Soner"
                [ Section.viewHorizontalGroup
                    [ Select.init "travelFromZone"
                        |> Select.setTitle (Just "Avreisesone")
                        |> Select.setOnInput (Just onFromZoneSelected)
                        |> Select.setDisabled disabled
                        |> Select.view (List.map (viewZone selectedFromZone) sortedZones)
                    , Select.init "travelToZone"
                        |> Select.setTitle (Just "Ankomstsone")
                        |> Select.setOnInput (Just onToZoneSelected)
                        |> Select.setDisabled disabled
                        |> Select.view (List.map (viewZone selectedToZone) sortedZones)
                    ]
                ]
            , Section.viewPaddedItem [ H.p [] [ H.a [ A.href appInfo.zoneMapUrl, A.target "_blank" ] [ H.text "Se sonekart og beskrivelser (åpner ny side)" ] ] ]
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| Utils.langString zone.name ]
