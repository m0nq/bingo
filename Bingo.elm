module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (encodeUri)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Random
import ViewHelpers exposing (..)


-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


initialModel : Model
initialModel =
    Model "Anonymous" 1 [] Nothing "" EnteringName



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            { model | gameState = state } ! []

        SaveName ->
            { model
                | name = model.nameInput
                , nameInput = ""
                , gameState = Playing
            }
                ! []

        CancelName ->
            { model
                | nameInput = ""
                , gameState = Playing
            }
                ! []

        SetNameInput value ->
            { model | nameInput = value } ! []

        NewRandom randomNumber ->
            { model | gameNumber = randomNumber } ! []

        ShareScore ->
            ( model, postScore model )

        NewScore result ->
            case result of
                Ok score ->
                    let
                        message =
                            "Your score of " ++ (toString score.score) ++ " was successfully shared!"
                    in
                        { model | alertMessage = Just message } ! []

                Err error ->
                    { model | alertMessage = Just (httpErrorToString error) } ! []

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    { model | entries = randomEntries } ! []

                Err error ->
                    { model | alertMessage = Just (httpErrorToString error) } ! []

        CloseAlert ->
            { model | alertMessage = Nothing } ! []

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries } ! []


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.NetworkError ->
            "Is the server running...?"

        Http.BadStatus response ->
            (toString response.status.message)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)



-- DECODERS


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        idDecoder
        phraseDecoder
        pointsDecoder
        booleanDecoder


idDecoder : Decoder Int
idDecoder =
    (field "id" Decode.int)


phraseDecoder : Decoder String
phraseDecoder =
    (field "phrase" Decode.string)


pointsDecoder : Decoder Int
pointsDecoder =
    (field "points" Decode.int)


booleanDecoder : Decoder Bool
booleanDecoder =
    (succeed False)


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



-- ENCODERS


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
        ]



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            "http://localhost:3000/scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request


getEntries : Cmd Msg
getEntries =
    let
        entriesUrl =
            "http://localhost:3000/random-entries"
    in
        (Decode.list entryDecoder)
            |> Http.get entriesUrl
            |> Http.send NewEntries



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alertMessage CloseAlert model.alertMessage
        , viewNameInput model
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , primaryButton ShareScore "Share Score"
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.nameInput
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]

        Playing ->
            text ""


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered by Elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
        ul [] listOfEntries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


viewScore : a -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
