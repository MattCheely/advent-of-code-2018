module Day6 exposing (coordinates, part1)

import Data.Day6 exposing (inputStr)
import Dict exposing (Dict)



-- Section: Input Pasring


coordinates : List ( Int, Int )
coordinates =
    inputStr
        |> String.lines
        |> List.map (String.split ",")
        |> List.map tupleFromList


tupleFromList : List String -> ( Int, Int )
tupleFromList intList =
    let
        first =
            List.head intList
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        second =
            List.tail intList
                |> Maybe.andThen List.head
                |> Maybe.map String.trim
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0
    in
    ( first, second )



-- Section: Part One


type alias Grid =
    { xMin : Int
    , xMax : Int
    , yMin : Int
    , yMax : Int
    }


part1 : List ( Int, Int ) -> Int
part1 coords =
    let
        gridBoundaries =
            findBoundaries coords
    in
    getAreas coords gridBoundaries
        |> Dict.values
        |> List.sortBy negate
        |> List.head
        |> Maybe.withDefault 0


getAreas : List ( Int, Int ) -> Grid -> Dict ( Int, Int ) Int
getAreas coords bounds =
    let
        xs =
            List.range bounds.xMin bounds.xMax

        ys =
            List.range bounds.yMin bounds.yMax

        emptyAreas =
            List.map (\coord -> ( coord, 0 )) coords
                |> Dict.fromList
    in
    foldxy
        (\x y areas ->
            case closestCoord ( x, y ) coords of
                Just coord ->
                    if inInfiniteRegion bounds ( x, y ) then
                        Dict.remove coord areas

                    else
                        Dict.update coord
                            (Maybe.map ((+) 1))
                            areas

                Nothing ->
                    areas
        )
        emptyAreas
        xs
        ys


inInfiniteRegion : Grid -> ( Int, Int ) -> Bool
inInfiniteRegion grid ( x, y ) =
    (x == grid.xMin)
        || (x == grid.xMax)
        || (y == grid.yMin)
        || (y == grid.yMax)


closestCoord : ( Int, Int ) -> List ( Int, Int ) -> Maybe ( Int, Int )
closestCoord loc coords =
    case coords of
        first :: rest ->
            List.foldl
                (replaceIfCloser loc)
                ( Just first, manhattanDistance loc first )
                rest
                |> Tuple.first

        [] ->
            Nothing


replaceIfCloser :
    ( Int, Int )
    -> ( Int, Int )
    -> ( Maybe ( Int, Int ), Int )
    -> ( Maybe ( Int, Int ), Int )
replaceIfCloser loc coord ( curClosest, shortestDistance ) =
    let
        distance =
            manhattanDistance loc coord
    in
    case compare distance shortestDistance of
        LT ->
            ( Just coord, distance )

        GT ->
            ( curClosest, shortestDistance )

        EQ ->
            ( Nothing, shortestDistance )


manhattanDistance : ( Int, Int ) -> ( Int, Int ) -> Int
manhattanDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


findBoundaries : List ( Int, Int ) -> Grid
findBoundaries coords =
    List.foldl
        (\( x, y ) grid ->
            { xMin = min x grid.xMin
            , xMax = max x grid.xMax
            , yMin = min y grid.yMin
            , yMax = max y grid.yMax
            }
        )
        { xMin = 9999, xMax = 0, yMin = 9999, yMax = 0 }
        coords


foldxy : (a -> b -> c -> c) -> c -> List a -> List b -> c
foldxy fun acc xs ys =
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
