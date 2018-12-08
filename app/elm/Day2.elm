module Day2 exposing (boxIds, partOne, partTwo)

import Data.Day2 exposing (inputStr)
import Dict exposing (Dict)



-- Section: Input Parsing


boxIds : List String
boxIds =
    inputStr
        |> String.split "\n"



-- Section: Part 1


type BoxCategory
    = HasTwo
    | HasThree
    | HasTwoAndThree
    | DontCare


partOne : List String -> Int
partOne boxes =
    let
        ( twos, threes ) =
            categoryCounts boxes
    in
    twos * threes


categoryCounts : List String -> ( Int, Int )
categoryCounts boxes =
    List.foldl
        (\boxId ( twos, threes ) ->
            case categorizeBox boxId of
                HasTwo ->
                    ( twos + 1, threes )

                HasThree ->
                    ( twos, threes + 1 )

                HasTwoAndThree ->
                    ( twos + 1, threes + 1 )

                DontCare ->
                    ( twos, threes )
        )
        ( 0, 0 )
        boxes


categorizeBox : String -> BoxCategory
categorizeBox boxId =
    let
        counts =
            String.foldl countChar Dict.empty boxId
    in
    Dict.foldl
        (\letter count category ->
            case ( category, count ) of
                ( DontCare, 2 ) ->
                    HasTwo

                ( DontCare, 3 ) ->
                    HasThree

                ( HasTwo, 3 ) ->
                    HasTwoAndThree

                ( HasThree, 2 ) ->
                    HasTwoAndThree

                ( _, _ ) ->
                    category
        )
        DontCare
        counts


countChar : Char -> Dict Char Int -> Dict Char Int
countChar char counts =
    Dict.update char
        (\count ->
            Maybe.withDefault 0 count
                |> (+) 1
                |> Just
        )
        counts



-- Section: Part 2


partTwo : List String -> String
partTwo boxes =
    case boxes of
        first :: rest ->
            case checkBoxAgainstOthers first rest of
                Just characters ->
                    characters

                Nothing ->
                    partTwo rest

        [] ->
            "Where my boxes at?"


checkBoxAgainstOthers : String -> List String -> Maybe String
checkBoxAgainstOthers box otherBoxes =
    case otherBoxes of
        otherBox :: remaining ->
            case
                checkBoxes
                    (String.toList box)
                    (String.toList otherBox)
                    []
            of
                Nothing ->
                    checkBoxAgainstOthers box remaining

                Just characters ->
                    Just characters

        [] ->
            Nothing


checkBoxes : List Char -> List Char -> List Char -> Maybe String
checkBoxes box1 box2 matches =
    case ( box1, box2 ) of
        ( [], _ ) ->
            Nothing

        ( _, [] ) ->
            Nothing

        ( char1 :: rest1, char2 :: rest2 ) ->
            if char1 /= char2 then
                if rest1 == rest2 then
                    List.reverse matches
                        ++ rest1
                        |> String.fromList
                        |> Just

                else
                    Nothing

            else
                checkBoxes rest1 rest2 (char1 :: matches)
