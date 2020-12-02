module Page.Home exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Ticket exposing (Offer, Ticket)
import Data.Webshop exposing (Profile, Token)
import Environment exposing (Environment)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Route exposing (Route)
import Service.Ticket as TicketService
import Service.Webshop as WebshopService
import Task
import Util.Task as TaskUtil


type Msg
    = FetchTickets
    | ReceiveTickets (Result Http.Error (List Ticket))
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | Hello
    | ReceiveHello (Result Http.Error ())
    | GetProfile
    | ReceiveProfile (Result Http.Error Profile)
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())
    | GetTokens
    | ReceiveTokens (Result Http.Error (List Token))
    | UpdateTravelCardId String
    | AddTravelCard
    | ReceiveAddTravelCard (Result Http.Error ())
    | AddQrCode
    | ReceiveAddQrCode (Result Http.Error ())
    | LoadAccount


type alias Model =
    { tickets : List Ticket
    , offers : List Offer
    , hello : String
    , profile : Maybe Profile
    , tokens : List Token
    , travelCardId : String
    , firstName : String
    , lastName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , offers = []
      , hello = ""
      , profile = Nothing
      , tokens = []
      , travelCardId = ""
      , firstName = ""
      , lastName = ""
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        FetchOffers ->
            ( model, fetchOffers env )

        ReceiveOffers result ->
            case result of
                Ok offers ->
                    ( { model | offers = offers }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        FetchTickets ->
            ( model
            , env.customerId
                |> Maybe.map (fetchTickets env)
                |> Maybe.withDefault Cmd.none
            )

        ReceiveTickets result ->
            case result of
                Ok tickets ->
                    ( { model | tickets = tickets }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        Hello ->
            ( { model | hello = "Trying to say hello..." }, fetchHello env )

        ReceiveHello result ->
            case result of
                Ok () ->
                    ( { model | hello = "Hello was OK" }, Cmd.none )

                Err _ ->
                    ( { model | hello = "Server refused to say hello :(" }, Cmd.none )

        GetProfile ->
            ( model, fetchProfile env )

        ReceiveProfile result ->
            case result of
                Ok profile ->
                    ( { model
                        | profile = Just profile
                        , firstName = profile.firstName
                        , lastName = profile.lastName
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        UpdateFirstName value ->
            ( { model | firstName = value }, Cmd.none )

        UpdateLastName value ->
            ( { model | lastName = value }, Cmd.none )

        UpdateProfile ->
            ( model, updateProfile env model.firstName model.lastName )

        ReceiveUpdateProfile result ->
            case result of
                Ok () ->
                    ( model, fetchProfile env )

                Err _ ->
                    ( model, Cmd.none )

        GetTokens ->
            ( model, fetchTokens env )

        ReceiveTokens result ->
            case result of
                Ok tokens ->
                    ( { model | tokens = tokens }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        UpdateTravelCardId value ->
            ( { model | travelCardId = value }, Cmd.none )

        AddTravelCard ->
            ( model, addTravelCard env model.travelCardId )

        ReceiveAddTravelCard result ->
            case result of
                Ok () ->
                    ( model, fetchTokens env )

                Err _ ->
                    ( model, Cmd.none )

        AddQrCode ->
            ( model, addQrCode env )

        ReceiveAddQrCode result ->
            case result of
                Ok () ->
                    ( model, fetchTokens env )

                Err _ ->
                    ( model, Cmd.none )

        LoadAccount ->
            ( model
            , Cmd.batch
                [ TaskUtil.doTask GetProfile
                , TaskUtil.doTask GetTokens
                , TaskUtil.doTask FetchTickets
                ]
            )


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view env _ model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Hello" ]
                , H.button [ E.onClick Hello ] [ H.text "Hello" ]
                , H.p [] [ H.text model.hello ]
                , H.h2 [] [ H.text "Offers" ]
                , H.button [ E.onClick FetchOffers ] [ H.text "Refresh" ]
                , H.ol [] <| List.map viewOffer model.offers
                , H.h2 [] [ H.text "Tickets" ]
                , H.button [ E.onClick FetchTickets ] [ H.text "Refresh" ]
                , H.ol [] <| List.map viewTicket model.tickets
                , H.h2 [] [ H.text "Profile" ]
                , H.button [ E.onClick GetProfile ] [ H.text "Refresh" ]
                , H.div [] [ viewProfile model.profile ]
                , H.h2 [] [ H.text "Update profile" ]
                , H.div []
                    [ H.text "First name: "
                    , H.input
                        [ A.value model.firstName
                        , E.onInput UpdateFirstName
                        , A.placeholder "First name"
                        ]
                        []
                    ]
                , H.div []
                    [ H.text "Last name: "
                    , H.input
                        [ A.value model.lastName
                        , E.onInput UpdateLastName
                        , A.placeholder "Last name"
                        ]
                        []
                    ]
                , H.button
                    [ A.disabled (String.trim model.firstName == "" || String.trim model.lastName == "")
                    , E.onClick UpdateProfile
                    ]
                    [ H.text "Update" ]
                , H.h2 [] [ H.text "Tokens" ]
                , H.button [ E.onClick GetTokens ] [ H.text "Refresh" ]
                , if List.length model.tokens == 0 then
                    H.p [] [ H.text "No tokens." ]

                  else
                    H.ol [] <| List.map viewToken model.tokens
                , H.h3 [] [ H.text "Add QR token" ]
                , H.button
                    [ E.onClick AddQrCode ]
                    [ H.text "Add" ]
                , H.h3 [] [ H.text "Add travel card" ]
                , H.input
                    [ A.value model.travelCardId
                    , E.onInput UpdateTravelCardId
                    , A.placeholder "Travel card id"
                    ]
                    []
                , H.button
                    [ A.disabled (String.trim model.travelCardId == "")
                    , E.onClick AddTravelCard
                    ]
                    [ H.text "Add" ]
                ]

        Nothing ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Not logged in" ]
                , H.p [] [ H.text "You need to log in." ]
                ]


viewOffer : Offer -> Html msg
viewOffer offer =
    H.div []
        [ H.div [] [ H.text offer.offerId ]
        , H.div [] [ H.text offer.travellerId ]
        , H.div []
            [ offer.prices
                |> List.head
                |> Maybe.map (\price -> price.currency ++ " " ++ price.amount)
                |> Maybe.withDefault "No prices"
                |> H.text
            ]
        ]


viewTicket : Ticket -> Html msg
viewTicket ticket =
    H.text "ticket"


viewProfile : Maybe Profile -> Html msg
viewProfile maybeProfile =
    case maybeProfile of
        Just profile ->
            H.ul []
                [ H.li [] [ H.text ("First name: " ++ profile.firstName) ]
                , H.li [] [ H.text ("Last name: " ++ profile.lastName) ]
                ]

        Nothing ->
            H.p [] [ H.text "No profile." ]


viewToken : Token -> Html msg
viewToken token =
    H.li []
        [ H.div [] [ H.text <| "Id: " ++ token.id ]
        , H.div [] [ H.text <| "Type: " ++ Debug.toString token.type_ ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


fetchTickets : Environment -> String -> Cmd Msg
fetchTickets env customerId =
    TicketService.getTicketList env customerId
        |> Http.toTask
        |> Task.attempt ReceiveTickets


fetchOffers : Environment -> Cmd Msg
fetchOffers env =
    TicketService.search env
        |> Http.toTask
        |> Task.attempt ReceiveOffers


fetchHello : Environment -> Cmd Msg
fetchHello env =
    WebshopService.hello env
        |> Http.toTask
        |> Task.attempt ReceiveHello


fetchProfile : Environment -> Cmd Msg
fetchProfile env =
    WebshopService.getProfile env
        |> Http.toTask
        |> Task.attempt ReceiveProfile


updateProfile : Environment -> String -> String -> Cmd Msg
updateProfile env firstName lastName =
    WebshopService.updateProfile env firstName lastName
        |> Http.toTask
        |> Task.attempt ReceiveUpdateProfile


fetchTokens : Environment -> Cmd Msg
fetchTokens env =
    WebshopService.getTokens env
        |> Http.toTask
        |> Task.attempt ReceiveTokens


addTravelCard : Environment -> String -> Cmd Msg
addTravelCard env id =
    WebshopService.addTravelCard env id
        |> Http.toTask
        |> Task.attempt ReceiveAddTravelCard


addQrCode : Environment -> Cmd Msg
addQrCode env =
    WebshopService.addQrCode env
        |> Http.toTask
        |> Task.attempt ReceiveAddQrCode
