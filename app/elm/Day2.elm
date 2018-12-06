module Day2 exposing (partOne, partTwo)

import Dict exposing (Dict)



-- Part 1


type BoxCategory
    = HasTwo
    | HasThree
    | HasTwoAndThree
    | DontCare


partOne : List String -> Int
partOne boxIds =
    let
        ( twos, threes ) =
            categoryCounts boxIds
    in
    twos * threes


categoryCounts : List String -> ( Int, Int )
categoryCounts boxIds =
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
        boxIds


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



-- Part 2


partTwo : List String -> String
partTwo boxIds =
    case boxIds of
        first :: rest ->
            case searchForMatch first rest of
                FabricBoxes box1 box2 ->
                    sharedChars box1 box2

                RandoBoxes ->
                    partTwo rest

        [] ->
            "Where my boxes at?"


type CheckResult
    = FabricBoxes String String
    | RandoBoxes


searchForMatch : String -> List String -> CheckResult
searchForMatch box otherBoxes =
    case otherBoxes of
        otherBox :: remaining ->
            case compareBoxes box otherBox of
                RandoBoxes ->
                    searchForMatch box remaining

                FabricBoxes box1 box2 ->
                    FabricBoxes box1 box2

        [] ->
            RandoBoxes


compareBoxes : String -> String -> CheckResult
compareBoxes box1 box2 =
    let
        differences =
            List.map2 (/=) (String.toList box1) (String.toList box2)
                |> List.filter identity
                |> List.length
    in
    if differences == 1 then
        FabricBoxes box1 box2

    else
        RandoBoxes


sharedChars : String -> String -> String
sharedChars box1 box2 =
    List.map2
        (\char1 char2 ->
            if char1 == char2 then
                Just char1

            else
                Nothing
        )
        (String.toList box1)
        (String.toList box2)
        |> List.filterMap identity
        |> String.fromList


searchForMatchAlt : String -> List String -> Maybe String
searchForMatchAlt box otherBoxes =
    case otherBoxes of
        otherBox :: remaining ->
            case
                doComparison
                    (String.toList box)
                    (String.toList otherBox)
                    []
                    False
            of
                Nothing ->
                    searchForMatchAlt box remaining

                Just characters ->
                    Just characters

        [] ->
            Nothing


doComparison : List Char -> List Char -> List Char -> Bool -> Maybe String
doComparison box1 box2 matches foundDiff =
    case ( box1, box2 ) of
        ( [], _ ) ->
            if foundDiff then
                Just (String.fromList matches)

            else
                Nothing

        ( _, [] ) ->
            if foundDiff then
                Just (String.fromList matches)

            else
                Nothing

        ( char1 :: rest1, char2 :: rest2 ) ->
            if char1 == char2 then
                if not foundDiff then
                    doComparison rest1 rest1 matches True

                else
                    Nothing

            else
                doComparison rest1 rest2 (char1 :: matches) foundDiff
