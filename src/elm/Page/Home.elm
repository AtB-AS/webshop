module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

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


type Msg
    = FetchTickets
    | ReceiveTickets (Result Http.Error (List Ticket))
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | Hello
    | ReceiveHello (Result Http.Error ())
    | GetProfile
    | ReceiveProfile (Result Http.Error Profile)
    | GetTokens
    | ReceiveTokens (Result Http.Error (List Token))
    | UpdateTravelCardId String
    | AddTravelCard
    | ReceiveAddTravelCard (Result Http.Error ())


type alias Model =
    { tickets : List Ticket
    , offers : List Offer
    , hello : String
    , profile : Maybe Profile
    , tokens : List Token
    , travelCardId : String
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , offers = []
      , hello = ""
      , profile = Nothing
      , tokens = []
      , travelCardId = ""
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
                    ( { model | profile = Just profile }, Cmd.none )

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
                , H.h2 [] [ H.text "Tokens" ]
                , H.button [ E.onClick GetTokens ] [ H.text "Refresh" ]
                , if List.length model.tokens == 0 then
                    H.p [] [ H.text "No tokens." ]

                  else
                    H.ol [] <| List.map viewToken model.tokens
                , H.h2 [] [ H.text "Add travel card" ]
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
                , H.ol [] <| List.map viewToken model.tokens
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
                , H.li [] [ H.text ("Email: " ++ profile.email) ]
                ]

        Nothing ->
            H.p [] [ H.text "No profile." ]


viewToken : Token -> Html msg
viewToken token =
    H.text token.id


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
