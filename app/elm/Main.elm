module Main exposing (main)

import Browser
import Day1
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Http exposing (get)
import RemoteData exposing (WebData)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    WebData (List Int)


init : () -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading
    , get
        { url = "/day1.txt"
        , expect = Http.expectString parseInput
        }
    )


parseInput : Result Http.Error String -> Msg
parseInput response =
    case response of
        Ok text ->
            String.split "\n" text
                |> List.filterMap String.toInt
                |> GotInput

        Err err ->
            LookupFailed err



-- Update


type Msg
    = GotInput (List Int)
    | LookupFailed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInput input ->
            ( RemoteData.Success input, Cmd.none )

        LookupFailed err ->
            ( RemoteData.Failure err, Cmd.none )



-- View


view model =
    dayView model Day1.part1 Day1.part2


dayView model partOne partTwo =
    div [ id "day-1" ]
        (case model of
            RemoteData.Success input ->
                [ div [ id "part-one" ]
                    [ text ("Part One: " ++ partOne input)
                    ]
                , div
                    [ id "part-two" ]
                    [ text ("Part Two: " ++ partTwo input)
                    ]
                ]

            RemoteData.Failure err ->
                [ text "Failed to fetch input." ]

            _ ->
                [ text "..." ]
        )
