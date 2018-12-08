module Day1 exposing (freqChanges, part1, part2)

import Data.Day1 exposing (inputStr)
import Set exposing (Set)



-- Section: Input Parsing


freqChanges : List Int
freqChanges =
    inputStr
        |> String.split "\n"
        |> List.filterMap String.toInt



-- Section: Part One


part1 : List Int -> String
part1 changes =
    List.foldl (+) 0 changes
        |> String.fromInt



-- Section: Part Two


part2 : List Int -> String
part2 changes =
    doPartTwo
        { changeList = changes
        , frequency = 0
        , pendingChanges = changes
        }
        Set.empty
        |> String.fromInt


doPartTwo : DeviceState -> Set Int -> Int
doPartTwo device seenFrequencies =
    if Set.member device.frequency seenFrequencies then
        device.frequency

    else
        doPartTwo
            (advanceDevice device)
            (Set.insert device.frequency seenFrequencies)


type alias DeviceState =
    { changeList : List Int
    , frequency : Int
    , pendingChanges : List Int
    }


advanceDevice : DeviceState -> DeviceState
advanceDevice { changeList, frequency, pendingChanges } =
    case pendingChanges of
        change :: rest ->
            { changeList = changeList
            , frequency = frequency + change
            , pendingChanges = rest
            }

        [] ->
            advanceDevice
                { changeList = changeList
                , frequency = frequency
                , pendingChanges = changeList
                }
