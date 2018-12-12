module Day11 exposing (part1, part2, serialNumber)

import Array exposing (Array)
import Dict exposing (Dict)



-- Section: Input Parsing


serialNumber : Int
serialNumber =
    1308



-- Section: Part One
--
--
{-
   (((x + 10) * y) + S)(x + 10)
   (xy + 10y + S)(x + 10)
   (x^2y + 10xy + Sx + 10xy + 100y + 10*S)
   (x^2y + 20xy + Sx + 100y + 10*S )
   y(x^2 + 20x + Sx + 100) + 10*S
-}


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
                |> Maybe.withDefault 0
                |> (+) power
        )
        0
        ( ( subGridX, subGridX + 2 )
        , ( subGridY, subGridY + 2 )
        )


get2d x y grid =
    Array.get (x - 1) grid
        |> Maybe.andThen (Array.get (y - 1))


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


part2 : Int -> ( Int, Int, Int )
part2 sn =
    let
        initialCalculations =
            Array.initialize 300
                (\x ->
                    Array.initialize 300
                        (\y ->
                            Dict.singleton 1 (getPowerLevel sn (x + 1) (y + 1))
                        )
                )
    in
    List.foldl
        calculatePowerForSize
        ( initialCalculations, ( 0, ( 0, 0, 0 ) ) )
        (List.range 2 3)
        |> Tuple.second
        |> Tuple.second


calculatePowerForSize :
    Int
    -> ( Array (Array (Dict Int Int)), ( Int, ( Int, Int, Int ) ) )
    -> ( Array (Array (Dict Int Int)), ( Int, ( Int, Int, Int ) ) )
calculatePowerForSize size ( powers, prevMax ) =
    let
        ( sourceSize, sourceFactor ) =
            getSourceGridInfo size (size - 1)
    in
    if False then
        powersFromFactor size sourceSize sourceFactor ( powers, prevMax )

    else
        powersFromPrevious size ( powers, prevMax )


getSourceGridInfo : Int -> Int -> ( Int, Int )
getSourceGridInfo size candidateSource =
    if candidateSource == 0 then
        ( 1, size )

    else if modBy size candidateSource == 0 then
        ( candidateSource, size // candidateSource )

    else
        getSourceGridInfo size (candidateSource - 1)


powersFromPrevious size ( powers, prevMax ) =
    foldxy
        (\x y ( powerData, maxPower ) ->
            let
                powerHere =
                    powerFromPrevious x y size powerData
            in
            ( addPower x y size powerHere powerData
            , if powerHere > Tuple.first maxPower then
                ( powerHere, ( x, y, size ) )

              else
                maxPower
            )
        )
        ( powers, prevMax )
        ( ( 1, 301 - size ), ( 1, 301 - size ) )


powerFromPrevious x y size powerData =
    let
        xMax =
            x + size - 1

        yMax =
            y + size - 1

        rightCoords =
            List.range y yMax
                |> List.map (Tuple.pair xMax)

        bottomCoords =
            List.range x (xMax - 1)
                |> List.map (\x1 -> ( x1, yMax ))
    in
    getPower x y (size - 1) powerData
        + List.foldl
            (\( x1, y1 ) power ->
                getPower x1 y1 1 powerData + power
            )
            0
            rightCoords
        + List.foldl
            (\( x1, y1 ) power ->
                getPower x1 y1 1 powerData + power
            )
            0
            bottomCoords


powersFromFactor :
    Int
    -> Int
    -> Int
    -> ( Array (Array (Dict Int Int)), ( Int, ( Int, Int, Int ) ) )
    -> ( Array (Array (Dict Int Int)), ( Int, ( Int, Int, Int ) ) )
powersFromFactor size sourceSize factor ( powers, prevMax ) =
    foldxy
        (\x y ( powerData, maxPower ) ->
            let
                powerHere =
                    powerFromFactor x y size sourceSize factor powerData
            in
            ( addPower x y size powerHere powerData
            , if powerHere > Tuple.first maxPower then
                ( powerHere, ( x, y, size ) )

              else
                maxPower
            )
        )
        ( powers, prevMax )
        ( ( 1, 301 - size ), ( 1, 301 - size ) )


powerFromFactor x y size sourceSize factor powerData =
    foldxy
        (\offsetX offsetY power ->
            let
                targetX =
                    x
                        + (sourceSize * offsetX)

                targetY =
                    y + (sourceSize * offsetY)
            in
            power + getPower targetX targetY sourceSize powerData
        )
        0
        ( ( 0, factor - 1 ), ( 0, factor - 1 ) )


getPower : Int -> Int -> Int -> Array (Array (Dict Int Int)) -> Int
getPower x y size powerData =
    get2d x y powerData
        |> Maybe.andThen (Dict.get size)
        |> Maybe.withDefault 0


addPower : Int -> Int -> Int -> Int -> Array (Array (Dict Int Int)) -> Array (Array (Dict Int Int))
addPower x y size power powerGrid =
    get2d x y powerGrid
        |> Maybe.withDefault Dict.empty
        |> Dict.insert size power
        |> (\data -> set2d x y data powerGrid)


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
