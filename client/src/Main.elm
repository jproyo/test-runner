module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewTestToRun)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import TestRunnerApi exposing (..)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL

statusToInt : TestStatus -> Int
statusToInt st = case st of 
    Passed -> 1
    Failed -> 2
    Running -> 3
    NotStartedYet -> 4

type alias Model =
    { tests : List TestToRun
    , result : Maybe TestsToRunResponse
    , error : Maybe String
    , submitted : Bool
    }


testsToRun : List TestToRun
testsToRun =
    [ TestToRun "commas are rotated properly" "generateDummyTest()"
    , TestToRun "exclamation points stand up straight" "generateDummyTest()"
    , TestToRun "run-on sentences don't run forever" "generateDummyTest()"
    , TestToRun "question marks curl down, not up" "generateDummyTest()"
    , TestToRun "semicolons are adequately waterproof" "generateDummyTest()"
    , TestToRun "capital letters can do yoga" "generateDummyTest()"
    ]


init : ( Model, Cmd Msg )
init =
    ( Model testsToRun Nothing Nothing False, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.result of
        Just r ->
            if r.generalStatus == Finished
                then Sub.none
                else Time.every 1000 Polling

        Nothing ->
            Sub.none



-- UPDATE


type Msg
    = FromServer FromServer
    | Polling Time.Posix
    | FromUi FromUi
    | Error String


type FromServer
    = SubmitTests TestsToRunResponse
    | GetStatus String
    | GetStatusResp TestsToRunResponse


type FromUi
    = SubmitTestsButton
    | Done TestsToRunResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Polling _ ->
            ( model
            , Maybe.withDefault Cmd.none (Maybe.map (\re -> getApiRunnerStatusByTestId re.testSetId (fromServer (\r -> SubmitTests r))) model.result)
            )

        FromServer fromServerMsg ->
            case fromServerMsg of
                SubmitTests testSubmitted ->
                    ( { model | result = Just testSubmitted }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FromUi fromUi ->
            case fromUi of
                SubmitTestsButton ->
                    ( { model | submitted = True }
                    , postApiRunnerNew model.tests (fromServer (\r -> SubmitTests r))
                    )

                Done _ ->
                    ( model, Cmd.none )

        Error error ->
            ( { model | error = Just error }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response



-- VIEW


hideClass : List ( String, String )
hideClass =
    [ ( "display", "none" ) ]


view : Model -> Html Msg
view model =
    let
        items =
            List.map viewTestToRun model.tests

        resultOrDefault =
            Maybe.withDefault (TestsToRunResponse NotStarted "NOTHING" []) model.result

        resultRunner =
            List.map viewTestResult (List.sortBy (\l -> statusToInt l.status) resultOrDefault.results)

        error =
            model.error
                |> Maybe.map viewError
                |> Maybe.withDefault (Html.text "")

        divTestRun =
            div
                [ hidden model.submitted ]
                [ span [ style "bold" "true" ] [ text "Test Runner" ]
                , ul [] items
                , button [ onClick (FromUi SubmitTestsButton) ] [ text "Run Test Set" ]
                ]

        divTestResult =
            div
                [ hidden (NotStarted == resultOrDefault.generalStatus) ]
                [ span [ style "bold" "true" ] [ text "Test Runner - Results" ]
                , span [ style "bold" "true" ] [ text ("Result - " ++ Debug.toString resultOrDefault.generalStatus) ]
                , ul [] resultRunner
                ]
    in
    div []
        [ divTestRun
        , divTestResult
        , error
        ]


viewTestResult : Test -> Html Msg
viewTestResult test =
    li []
        [ text test.id
        , text " - "
        , text test.description
        , text " - "
        , text (Debug.toString test.status)
        ]


viewTestToRun : TestToRun -> Html Msg
viewTestToRun test =
    li []
        [ text test.description
        , text " - "
        , text test.run
        ]


viewError : String -> Html msg
viewError error =
    div
        []
        [ text <| "Error: " ++ error ]
