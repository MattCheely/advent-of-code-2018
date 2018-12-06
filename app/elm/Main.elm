module Main exposing (main)

import Browser
import Data.Day1 as Day1
import Data.Day2 as Day2
import Day1
import Day2
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (id)


main : Html msg
main =
    div []
        [ h1 [] [ text "Advent of code 2018" ]
        , dayView day2
        ]


dayView day =
    div []
        [ div [ id "part-one" ]
            [ h2 [] [ text "Part One" ]
            , text day.partOne
            ]
        , div [ id "part-two" ]
            [ h2 [] [ text "Part Two" ]
            , text day.partTwo
            ]
        ]


day2 =
    { partOne =
        Day2.partOne Day2.boxIds
            |> String.fromInt
    , partTwo =
        Day2.partTwo Day2.boxIds
    }


day1 =
    { partOne = Day1.part1 Day1.input
    , partTwo = Day1.part2 Day1.input
    }
