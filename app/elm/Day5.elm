module Day5 exposing (part1, part2, polymerStr)

import Data.Day5 exposing (inputStr, testStr)



-- Section: Input parsing


polymerStr =
    inputStr



-- Section: Part One


part1 : String -> Int
part1 polymer =
    doReaction ( [], String.toList polymer )
        |> List.length


doReaction : ( List Char, List Char ) -> List Char
doReaction ( previous, remaining ) =
    case ( previous, remaining ) of
        ( firstChar :: firstRemain, nextChar :: nextRemain ) ->
            if willReact firstChar nextChar then
                doReaction ( firstRemain, nextRemain )

            else
                doReaction ( nextChar :: previous, nextRemain )

        ( [], first :: rest ) ->
            doReaction ( [ first ], rest )

        ( _, [] ) ->
            previous


willReact : Char -> Char -> Bool
willReact c1 c2 =
    (c1 /= c2) && (Char.toLower c1 == Char.toLower c2)



-- Section: Part Two


part2 : String -> Int
part2 polymer =
    List.range 65 90
        |> List.map Char.fromCode
        -- I suspect there's a way to do this wihtout running the reaction 26 times
        |> List.map (sizeWithExclusion (String.toList polymer))
        |> List.minimum
        |> Maybe.withDefault 0


sizeWithExclusion : List Char -> Char -> Int
sizeWithExclusion polymer excludedChar =
    doReactionWithExclusion excludedChar ( [], polymer )
        |> List.length


doReactionWithExclusion : Char -> ( List Char, List Char ) -> List Char
doReactionWithExclusion excludedChar ( previous, remaining ) =
    let
        shouldExclude =
            \char -> Char.toLower char == Char.toLower excludedChar
    in
    case ( previous, remaining ) of
        ( firstChar :: firstRemain, nextChar :: nextRemain ) ->
            case ( shouldExclude firstChar, shouldExclude nextChar ) of
                ( True, True ) ->
                    doReactionWithExclusion excludedChar ( firstRemain, nextRemain )

                ( True, False ) ->
                    doReactionWithExclusion excludedChar ( firstRemain, remaining )

                ( False, True ) ->
                    doReactionWithExclusion excludedChar ( previous, nextRemain )

                ( False, False ) ->
                    doReactionWithExclusion
                        excludedChar
                        (reactOnce firstChar nextChar firstRemain nextRemain)

        ( [], first :: rest ) ->
            doReactionWithExclusion excludedChar ( [ first ], rest )

        ( _, [] ) ->
            previous


reactOnce : Char -> Char -> List Char -> List Char -> ( List Char, List Char )
reactOnce char1 char2 previous remaining =
    if willReact char1 char2 then
        ( previous, remaining )

    else
        ( char2 :: char1 :: previous, remaining )
