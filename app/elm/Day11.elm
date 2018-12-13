module Day11 exposing (part1, part2, serialNumber)

import Array exposing (Array)
import Dict exposing (Dict)



-- Section: Input Parsing


serialNumber : Int
serialNumber =
    1308



-- Section: Part One


part1 : Int -> ( Int, Int )
part1 sn =
    let
        grid =
            Array.initialize 300
                (\x ->
                    Array.initialize 300
                        (\y ->
                            getPowerLevel sn (x + 1) (y + 1)
                        )
                )
    in
    getAll3byPowers grid
        |> List.sortBy (Tuple.second >> negate)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault ( -1, -1 )


getAll3byPowers grid =
    foldxy
        (\x y levels ->
            let
                power =
                    get3byPower grid x y
            in
            ( ( x, y ), power ) :: levels
        )
        []
        ( ( 1, 298 ), ( 1, 298 ) )


get3byPower grid subGridX subGridY =
    foldxy
        (\x y power ->
            get2d x y grid
                |> (+) power
        )
        0
        ( ( subGridX, subGridX + 2 )
        , ( subGridY, subGridY + 2 )
        )


get2d : Int -> Int -> Array (Array Int) -> Int
get2d x y grid =
    Array.get (x - 1) grid
        |> Maybe.andThen (Array.get (y - 1))
        |> Maybe.withDefault 0


foldxy :
    (Int -> Int -> c -> c)
    -> c
    -> ( ( Int, Int ), ( Int, Int ) )
    -> c
foldxy fun acc ( ( xMin, xMax ), ( yMin, yMax ) ) =
    let
        xs =
            List.range xMin xMax

        ys =
            List.range yMin yMax
    in
    List.foldl
        (\x accOuter ->
            List.foldl
                (\y accInner ->
                    fun x y accInner
                )
                accOuter
                ys
        )
        acc
        xs


getPowerLevel : Int -> Int -> Int -> Int
getPowerLevel sn x y =
    let
        rackId =
            10 + x
    in
    ((rackId * y) + sn)
        * rackId
        |> getHundreds
        |> (\p -> p - 5)


getHundreds : Int -> Int
getHundreds num =
    -- Interger math is crazy, yo!
    (num // 100) - ((num // 1000) * 10)



-- Section: Part Two


part2 sn =
    let
        gridSize =
            300

        powerLevels =
            Array.initialize gridSize
                (\x ->
                    Array.initialize gridSize
                        (\y ->
                            getPowerLevel sn (x + 1) (y + 1)
                        )
                )

        sums =
            foldxy gradientAtPos
                ( 0, powerLevels )
                ( ( 1, gridSize ), ( 1, gridSize ) )
                |> Tuple.second
    in
    findMaxPower sums


findMaxPower : Array (Array Int) -> ( Int, Int, Int )
findMaxPower sums =
    let
        sizes =
            List.range 2 300
    in
    foldxy
        (\x y highestPower ->
            let
                biggestSize =
                    301 - max x y
            in
            List.foldl
                (\size max ->
                    let
                        power =
                            squarePower sums x y size
                    in
                    if power > Tuple.first max then
                        ( power, ( x, y, size ) )

                    else
                        max
                )
                highestPower
                (List.range 2 biggestSize)
        )
        ( 0, ( 0, 0, 0 ) )
        ( ( 1, 300 ), ( 1, 300 ) )
        |> Tuple.second


squarePower : Array (Array Int) -> Int -> Int -> Int -> Int
squarePower gradient x y size =
    let
        xMax =
            x + size - 1

        yMax =
            y + size - 1
    in
    get2d xMax yMax gradient
        - get2d (x - 1) yMax gradient
        - get2d xMax (y - 1) gradient
        + get2d (x - 1) (y - 1) gradient


gradientAtPos : Int -> Int -> ( Int, Array (Array Int) ) -> ( Int, Array (Array Int) )
gradientAtPos x y ( columnTotal, gradient ) =
    let
        gridSize =
            Array.length gradient

        newColTotal =
            columnTotal
                + get2d x y gradient

        upLeftTotal =
            newColTotal
                + get2d (x - 1) y gradient

        nextColumnTotal =
            if y == gridSize then
                0

            else
                newColTotal
    in
    ( nextColumnTotal, set2d x y upLeftTotal gradient )


set2d : Int -> Int -> a -> Array (Array a) -> Array (Array a)
set2d x y data grid =
    let
        xIdx =
            x - 1

        yIdx =
            y - 1
    in
    Array.get xIdx grid
        |> Maybe.map (Array.set yIdx data)
        |> Maybe.map (\col -> Array.set xIdx col grid)
        |> Maybe.withDefault grid
