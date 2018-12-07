module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Data.Day1 as Day1
import Data.Day2 as Day2
import Day1
import Day2
import Html exposing (Html, a, button, code, div, h1, h2, nav, p, pre, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import SyntaxHighlight as Highlight
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), int, s)


solve : Int -> DayPart -> String
solve dayNum dayPart =
    case dayNum of
        1 ->
            case dayPart of
                One ->
                    Day1.part1 Day1.input

                Two ->
                    Day1.part2 Day1.input

        2 ->
            case dayPart of
                One ->
                    Day2.partOne Day2.boxIds |> String.fromInt

                Two ->
                    Day2.partTwo Day2.boxIds

        _ ->
            "lowut"



-- Model


solutions =
    Array.fromList
        [ emptySolution
        , emptySolution
        ]


emptySolution =
    { part1 =
        { code = ""
        , solution = Nothing
        }
    , part2 =
        { code = ""
        , solution = Nothing
        }
    }


type alias Solution =
    { part1 :
        { code : String
        , solution : Maybe String
        }
    , part2 :
        { code : String
        , solution : Maybe String
        }
    }


type alias Model =
    { key : Navigation.Key
    , route : Route
    , days : Array Solution
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseRoute url
    in
    handleRoute
        { key = key
        , route = Intro
        , days = solutions
        }
        route


type DayPart
    = One
    | Two



-- Navigation


type Route
    = Intro
    | Day Int


parseRoute : Url -> Route
parseRoute url =
    let
        fragmentUrl =
            { url | path = url.fragment |> Maybe.withDefault "" }
    in
    Url.parse routeParser fragmentUrl
        |> Maybe.withDefault Intro


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map Day (s "day" </> int)
        ]



-- Update


type Msg
    = NavRequest UrlRequest
    | RouteChange Route
    | GotCode Int (Result Http.Error String)
    | RunProblem Int DayPart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavRequest req ->
            case req of
                Internal url ->
                    ( model, Navigation.load (Url.toString url) )

                External url ->
                    ( model, Navigation.load url )

        RouteChange route ->
            handleRoute model route

        GotCode day result ->
            case result of
                Ok code ->
                    ( { model | days = setDayCode model.days day code }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        RunProblem dayNum dayPart ->
            ( solve dayNum dayPart
                |> setSolution model dayNum dayPart
            , Cmd.none
            )


handleRoute : Model -> Route -> ( Model, Cmd Msg )
handleRoute model route =
    case route of
        Day id ->
            ( { model | route = route }, getDayCode id )

        Intro ->
            ( { model | route = route }, Cmd.none )


getDayCode : Int -> Cmd Msg
getDayCode day =
    let
        url =
            "/Day" ++ String.fromInt day ++ ".elm"
    in
    Http.get
        { url = url
        , expect = Http.expectString (GotCode day)
        }


setDayCode : Array Solution -> Int -> String -> Array Solution
setDayCode days targetDay code =
    let
        dayIdx =
            targetDay - 1
    in
    case Array.get dayIdx days of
        Just day ->
            Array.set dayIdx (setSolutionCode code day) days

        Nothing ->
            days


setSolutionCode : String -> Solution -> Solution
setSolutionCode code solution =
    let
        part1 =
            solution.part1

        part2 =
            solution.part2

        codeParts =
            String.split "-- Part" code
                |> List.drop 1
                |> List.take 2

        newPart1 =
            { part1
                | code =
                    List.head codeParts
                        |> Maybe.map String.lines
                        |> Maybe.map (List.drop 3)
                        |> Maybe.map (String.join "\n")
                        |> Maybe.withDefault "No Solution"
            }

        newPart2 =
            { part1
                | code =
                    List.tail codeParts
                        |> Maybe.andThen List.head
                        |> Maybe.map String.lines
                        |> Maybe.map (List.drop 3)
                        |> Maybe.map (String.join "\n")
                        |> Maybe.withDefault "No Solution"
            }
    in
    { solution | part1 = newPart1, part2 = newPart2 }


setSolution : Model -> Int -> DayPart -> String -> Model
setSolution model dayNum dayPart answer =
    let
        dayIdx =
            dayNum - 1

        newDay =
            Array.get dayIdx model.days
                |> Maybe.map (setDaySolution dayPart answer)
    in
    case newDay of
        Just solution ->
            { model | days = Array.set dayIdx solution model.days }

        Nothing ->
            model


setDaySolution : DayPart -> String -> Solution -> Solution
setDaySolution dayPart answer solution =
    case dayPart of
        One ->
            let
                part1 =
                    solution.part1

                newPart1 =
                    { part1 | solution = Just answer }
            in
            { solution | part1 = newPart1 }

        Two ->
            let
                part2 =
                    solution.part2

                newPart2 =
                    { part2 | solution = Just answer }
            in
            { solution | part2 = newPart2 }



-- View


view : Model -> Document Msg
view model =
    let
        ( title, content ) =
            case model.route of
                Intro ->
                    ( "Elm Advent of Code 2018", introView )

                Day day ->
                    ( "Day " ++ String.fromInt day, dayView day model.days )
    in
    { title = title
    , body = [ navView, content ]
    }


navView : Html Msg
navView =
    nav []
        (List.range 1 2
            |> List.map (String.fromInt >> dayLink)
            |> List.intersperse (span [ class "nav-divider" ] [])
        )


dayLink : String -> Html Msg
dayLink day =
    a [ class "navLink", href ("#/day/" ++ day) ]
        [ text ("Day " ++ day) ]


introView : Html Msg
introView =
    p [] [ text "Choose a day to see the solutions" ]


dayView : Int -> Array Solution -> Html Msg
dayView dayNum days =
    case Array.get (dayNum - 1) days of
        Just day ->
            div []
                [ h1 [] [ text ("Day " ++ String.fromInt dayNum) ]
                , partHeader One dayNum day
                , codeView day.part1.code
                , partHeader Two dayNum day
                , codeView day.part2.code
                ]

        Nothing ->
            text "What day is it?"


partHeader : DayPart -> Int -> Solution -> Html Msg
partHeader dayPart dayNum day =
    let
        ( dayText, solution ) =
            case dayPart of
                One ->
                    ( "One", day.part1.solution )

                Two ->
                    ( "Two", day.part2.solution )
    in
    h2 []
        [ text ("Part " ++ dayText ++ " ")
        , solutionView dayPart dayNum solution
        ]


solutionView : DayPart -> Int -> Maybe String -> Html Msg
solutionView dayPart dayNum solution =
    case solution of
        Just string ->
            span [] [ text string ]

        Nothing ->
            button [ onClick (RunProblem dayNum dayPart) ] [ text "Run" ]


codeView : String -> Html Msg
codeView codeStr =
    div []
        [ Highlight.useTheme Highlight.oneDark
        , Highlight.elm codeStr
            |> Result.map (Highlight.toBlockHtml Nothing)
            |> Result.withDefault (code [] [ text codeStr ])
        ]



-- Program


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlRequest = NavRequest
        , onUrlChange = parseRoute >> RouteChange
        , subscriptions = always Sub.none
        }
