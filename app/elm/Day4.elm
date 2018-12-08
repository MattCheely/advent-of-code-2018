module Day4 exposing (parsedShifts, part1, part2)

import Array exposing (Array)
import Data.Day4 exposing (inputStr, testStr)
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompUntil
        , chompUntilEndOr
        , int
        , keyword
        , loop
        , spaces
        , succeed
        , symbol
        )
import Parser.Extras exposing (many, some)



-- Section: Input Parsing


parsedShifts : List Shift
parsedShifts =
    String.lines inputStr
        |> List.sort
        -- Put timstamp on the end to simplify parsing
        |> List.map reverseTimestamp
        |> String.join "\n"
        |> Parser.run (many shiftParser)
        |> Result.withDefault []


reverseTimestamp : String -> String
reverseTimestamp entry =
    String.split "] " entry
        |> List.reverse
        |> String.join " "
        |> (\str -> str ++ "]")


minuteParser : Parser Int
minuteParser =
    succeed identity
        |. symbol "["
        |. int
        |. symbol "-"
        |. paddedInt
        |. symbol "-"
        |. paddedInt
        |. spaces
        |. paddedInt
        |. symbol ":"
        |= paddedInt
        |. symbol "]"


paddedInt : Parser Int
paddedInt =
    Parser.oneOf
        [ succeed identity
            |. symbol "0"
            |= int
        , int
        ]


guardParser : Parser Int
guardParser =
    succeed identity
        |. keyword "Guard"
        |. spaces
        |. symbol "#"
        |= int
        |. spaces
        |. keyword "begins shift"
        |. spaces
        |. minuteParser


napParser : Parser ( Int, Int )
napParser =
    succeed Tuple.pair
        |. keyword "falls asleep"
        |. spaces
        |= minuteParser
        |. spaces
        |. keyword "wakes up"
        |. spaces
        |= minuteParser


type alias Shift =
    { guard : Int
    , naps : List ( Int, Int )
    }


shiftParser : Parser Shift
shiftParser =
    succeed Shift
        |= guardParser
        |. spaces
        |= many napParser



-- Section: Part One : 151754


type alias GuardStats =
    { minutesAsleep : Int
    , frequencyAsleep : Array Int
    }


getSleepiestMinute : GuardStats -> ( Int, Int )
getSleepiestMinute guardStats =
    guardStats.frequencyAsleep
        |> Array.toIndexedList
        |> List.sortBy (Tuple.second >> negate)
        |> List.head
        |> Maybe.withDefault ( 99, 0 )


part1 : List Shift -> Int
part1 shifts =
    let
        allGuardStats =
            List.foldl addShiftStats Dict.empty shifts

        ( sleepiestGuard, sleepiestStats ) =
            Dict.foldl
                (\id stats ( guard, sleepiest ) ->
                    if stats.minutesAsleep > sleepiest.minutesAsleep then
                        ( id, stats )

                    else
                        ( guard, sleepiest )
                )
                ( 0, emptyGuardStats )
                allGuardStats

        sleepiestMinute =
            sleepiestStats.frequencyAsleep
                |> Array.toIndexedList
                |> List.sortBy Tuple.second
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( 99, 0 )
                |> Tuple.first
    in
    sleepiestGuard * sleepiestMinute


addShiftStats : Shift -> Dict Int GuardStats -> Dict Int GuardStats
addShiftStats shift stats =
    let
        guardStats =
            Dict.get shift.guard stats
                |> Maybe.withDefault emptyGuardStats

        newGuardStats =
            List.foldl collectNapStats guardStats shift.naps
    in
    Dict.insert shift.guard newGuardStats stats


collectNapStats : ( Int, Int ) -> GuardStats -> GuardStats
collectNapStats ( startMin, endMin ) guardStats =
    let
        napLength =
            endMin - startMin
    in
    { guardStats
        | minutesAsleep = guardStats.minutesAsleep + napLength
        , frequencyAsleep =
            List.foldl incrementAt
                guardStats.frequencyAsleep
                (List.range startMin (endMin - 1))
    }


incrementAt : Int -> Array Int -> Array Int
incrementAt idx array =
    Array.get idx array
        |> Maybe.withDefault 0
        |> (+) 1
        |> (\count -> Array.set idx count array)


emptyGuardStats : GuardStats
emptyGuardStats =
    { minutesAsleep = 0, frequencyAsleep = Array.repeat 50 0 }



-- Section: Part Two : 19896


part2 : List Shift -> Int
part2 shifts =
    let
        allGuardStats =
            List.foldl addShiftStats Dict.empty shifts
                |> Dict.toList

        ( id, ( minute, timeAsleep ) ) =
            List.map
                (\( guard, stats ) -> ( guard, getSleepiestMinute stats ))
                allGuardStats
                |> List.sortBy (Tuple.second >> Tuple.second >> negate)
                |> List.head
                |> Maybe.withDefault ( 0, ( 99, 0 ) )
    in
    id * minute
