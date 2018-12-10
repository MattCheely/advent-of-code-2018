module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Data.Day2 as Day2
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Html exposing (Html, a, button, code, div, h1, h2, nav, p, pre, span, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import SyntaxHighlight as Highlight
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), int, s)


solvers : Array (Array (() -> String))
solvers =
    [ [ \_ -> Day1.part1 Day1.freqChanges
      , \_ -> Day1.part2 Day1.freqChanges
      ]
    , [ \_ -> Day2.partOne Day2.boxIds |> String.fromInt
      , \_ -> Day2.partTwo Day2.boxIds
      ]
    , [ \_ -> Day3.part1 Day3.claims |> String.fromInt
      , \_ -> Day3.part2 Day3.claims |> String.fromInt
      ]
    , [ \_ -> Day4.part1 Day4.parsedShifts |> String.fromInt
      , \_ -> Day4.part2 Day4.parsedShifts |> String.fromInt
      ]
    , [ \_ -> Day5.part1 Day5.polymerStr |> String.fromInt
      , \_ -> Day5.part2 Day5.polymerStr |> String.fromInt
      ]
    , [ \_ -> Day6.part1 Day6.coordinates |> String.fromInt
      , \_ -> Day6.part2 Day6.coordinates |> String.fromInt
      ]
    , [ \_ -> Day7.part1 Day7.dependencies
      , \_ -> Day7.part2 Day7.dependencies |> String.fromInt
      ]
    , [ \_ -> Day8.part1 Day8.tree |> String.fromInt
      , \_ -> Day8.part2 Day8.tree |> String.fromInt
      ]
    , [ \_ -> Day9.part1 Day9.initialGame |> String.fromInt
      , \_ -> Day9.part1 Day9.initialGame2 |> String.fromInt
      ]
    ]
        |> List.map Array.fromList
        |> Array.fromList


solve : Int -> DayPart -> String
solve dayNum dayPart =
    let
        partIdx =
            case dayPart of
                One ->
                    0

                Two ->
                    1

        solver =
            Array.get (dayNum - 1) solvers
                |> Maybe.andThen (Array.get partIdx)
                |> Maybe.withDefault (always "Not solved")
    in
    solver ()



-- Model


type alias Model =
    { key : Navigation.Key
    , route : Route
    , days : Array Solution
    }


type alias Solution =
    { part1 : Part
    , part2 : Part
    , parseCode : String
    }


type alias Part =
    { code : String
    , solution : Maybe String
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
        , days = Array.repeat (Array.length solvers) emptySolution
        }
        route


emptySolution =
    let
        emptyPart =
            { code = ""
            , solution = Nothing
            }
    in
    { part1 = emptyPart
    , part2 = emptyPart
    , parseCode = ""
    }


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
            "Day" ++ String.fromInt day ++ ".elm"
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
            String.split "-- Section:" code
                |> List.drop 1
                |> List.take 3
                |> Array.fromList

        inputParsing =
            Array.get 0 codeParts
                |> Maybe.map cleanCodeSample
                |> Maybe.withDefault "No code found"

        newPart1 =
            { part1
                | code =
                    Array.get 1 codeParts
                        |> Maybe.map cleanCodeSample
                        |> Maybe.withDefault "No Solution"
            }

        newPart2 =
            { part1
                | code =
                    Array.get 2 codeParts
                        |> Maybe.map cleanCodeSample
                        |> Maybe.withDefault "No Solution"
            }
    in
    { solution
        | parseCode = inputParsing
        , part1 = newPart1
        , part2 = newPart2
    }


cleanCodeSample : String -> String
cleanCodeSample code =
    String.lines code
        |> List.drop 3
        |> String.join "\n"


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
    , body = [ navView model.days, content ]
    }


navView : Array Solution -> Html Msg
navView days =
    nav [ class "main-nav" ]
        (navLink "Home" "#"
            :: (List.range 1 (Array.length days)
                    |> List.map dayLink
               )
            |> List.intersperse (span [ class "nav-divider" ] [])
        )


dayLink : Int -> Html Msg
dayLink day =
    let
        dayStr =
            String.fromInt day
    in
    navLink ("Day " ++ dayStr) ("#/day/" ++ dayStr)


navLink : String -> String -> Html Msg
navLink content location =
    a [ class "navLink", href location ]
        [ text content ]


introView : Html Msg
introView =
    div [ class "eighty-col" ]
        [ p []
            [ text "These are my solutions for the "
            , a [ href "https://adventofcode.com/2018" ] [ text "2018 Advent of Code" ]
            , text """
        in Elm. I've had fun working on them. They've been good exercise
        for some of the things I do less often in Elm, like writing
        recursive funtions and parsers. """
            ]
        , p [] [ text """
        If you're here trying to evaluate Elm as a language, I want to
        note that these sorts of problems don't highlight the things I
        like best about Elm (also, how did you even find this?). It's
        not that Elm isn't a good tool for
        solving these kinds of problems. It's fine in that regard, and on
        some problems the pure functional approach lends itself to some
        really elegant solutions.

        However, Elm really shines when
        dealing with things like managing changing application state and
        handling unreliable input, none of which are present here.
        """ ]
        ]


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
                , parsingView day.parseCode
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


parsingView : String -> Html Msg
parsingView code =
    div []
        [ h2 [] [ text "Input Parsing" ]
        , codeView code
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
