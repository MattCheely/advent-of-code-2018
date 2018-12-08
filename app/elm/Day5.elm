module Day5 exposing (part1, polymerStr)

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
