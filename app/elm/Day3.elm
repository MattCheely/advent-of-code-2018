module Day3 exposing (claims, part1, part2)

import Array exposing (Array)
import Data.Day3 exposing (inputStr, testStr)
import Parser exposing ((|.), (|=), Parser, int, spaces, succeed, symbol)
import Set exposing (Set)



-- Input Parsing


type alias Claim =
    { id : Int
    , top : Int
    , left : Int
    , bottom : Int
    , right : Int
    , width : Int
    , height : Int
    }


buildClaim : Int -> Int -> Int -> Int -> Int -> Claim
buildClaim id left top width height =
    { id = id
    , top = top
    , left = left
    , bottom = top + height - 1
    , right = left + width - 1
    , width = width
    , height = height
    }


claimParser : Parser Claim
claimParser =
    succeed buildClaim
        |. symbol "#"
        |= int
        |. spaces
        |. symbol "@"
        |. spaces
        |= int
        |. symbol ","
        |= int
        |. symbol ":"
        |. spaces
        |= int
        |. symbol "x"
        |= int


claims : List Claim
claims =
    String.lines inputStr
        |> List.filterMap (Parser.run claimParser >> Result.toMaybe)



-- Part One: 109143


part1 : List Claim -> Int
part1 claimList =
    let
        markedFabric =
            markAllOverlaps claims emptyFabric
    in
    countMarks markedFabric


type alias Fabric =
    Array (Array Int)


emptyFabric : Fabric
emptyFabric =
    Array.repeat 1000 0
        |> Array.repeat 1000


markCell : Int -> Int -> Fabric -> Fabric
markCell x y fabric =
    Array.get y fabric
        |> Maybe.map (Array.set x 1)
        |> Maybe.map (\row -> Array.set y row fabric)
        |> Maybe.withDefault fabric


countMarks : Fabric -> Int
countMarks fabric =
    fabric
        |> Array.map (Array.foldl (+) 0)
        |> Array.foldl (+) 0


markAllOverlaps : List Claim -> Fabric -> Fabric
markAllOverlaps claimList fabric =
    case claimList of
        claim1 :: remaining ->
            markClaimOverlaps
                claim1
                remaining
                (markAllOverlaps remaining fabric)

        [] ->
            fabric


markClaimOverlaps : Claim -> List Claim -> Fabric -> Fabric
markClaimOverlaps claim1 otherClaims fabric =
    List.foldl
        (\claim2 innerFabric ->
            case getOverlap claim1 claim2 of
                Just overlap ->
                    markOverlaps overlap innerFabric

                Nothing ->
                    innerFabric
        )
        fabric
        otherClaims


type alias Overlap =
    { top : Int
    , bottom : Int
    , left : Int
    , right : Int
    }


getOverlap : Claim -> Claim -> Maybe Overlap
getOverlap claim1 claim2 =
    let
        hOverlap =
            horizontalOverlap claim1 claim2

        vOverlap =
            verticalOverlap claim1 claim2
    in
    case ( hOverlap, vOverlap ) of
        ( Just ( xMin, xMax ), Just ( yMin, yMax ) ) ->
            Just
                { top = xMin
                , bottom = xMax
                , left = yMin
                , right = yMax
                }

        ( _, _ ) ->
            Nothing


verticalOverlap : Claim -> Claim -> Maybe ( Int, Int )
verticalOverlap claim1 claim2 =
    if claim1.top <= claim2.bottom && claim2.bottom <= claim1.bottom then
        Just ( max claim1.top claim2.top, claim2.bottom )

    else if claim2.top <= claim1.bottom && claim1.bottom <= claim2.bottom then
        Just ( max claim2.top claim1.top, claim1.bottom )

    else if claim2.top <= claim1.top && claim1.top <= claim2.bottom then
        Just ( claim1.top, min claim2.bottom claim1.bottom )

    else if claim1.top <= claim2.top && claim2.top <= claim1.bottom then
        Just ( claim2.top, min claim1.bottom claim2.bottom )

    else
        Nothing


horizontalOverlap : Claim -> Claim -> Maybe ( Int, Int )
horizontalOverlap claim1 claim2 =
    if claim1.left <= claim2.right && claim2.right <= claim1.right then
        Just ( max claim1.left claim2.left, claim2.right )

    else if claim2.left <= claim1.right && claim1.right <= claim2.left then
        Just ( max claim2.left claim1.left, claim1.right )

    else if claim2.left <= claim1.left && claim1.left <= claim2.right then
        Just ( claim1.left, min claim2.right claim1.right )

    else if claim1.left <= claim2.left && claim2.left <= claim1.right then
        Just ( claim2.left, min claim1.right claim2.right )

    else
        Nothing


markOverlaps : Overlap -> Fabric -> Fabric
markOverlaps { top, bottom, left, right } fabric =
    List.foldl
        (\x fabricX ->
            List.foldl
                (\y fabricY -> markCell x y fabricY)
                fabricX
                (List.range top bottom)
        )
        fabric
        (List.range left right)



-- Part Two


part2 : List Claim -> Int
part2 claimList =
    let
        claimIds =
            List.map .id claimList
                |> Set.fromList
    in
    removeOverlappingClaims claimList claimIds
        |> Set.toList
        |> List.head
        |> Maybe.withDefault 0


removeOverlappingClaims : List Claim -> Set Int -> Set Int
removeOverlappingClaims claimList ids =
    case claimList of
        claim :: remaining ->
            Set.intersect
                (removeClaimOverlaps claim remaining ids)
                (removeOverlappingClaims remaining ids)

        [] ->
            ids


removeClaimOverlaps : Claim -> List Claim -> Set Int -> Set Int
removeClaimOverlaps claim1 otherClaims ids =
    List.foldl
        (removeIfOverlapping claim1)
        ids
        otherClaims


removeIfOverlapping : Claim -> Claim -> Set Int -> Set Int
removeIfOverlapping claim1 claim2 ids =
    if hasOverlaps claim1 claim2 then
        Set.remove claim1.id ids
            |> Set.remove claim2.id

    else
        ids


hasOverlaps : Claim -> Claim -> Bool
hasOverlaps claim1 claim2 =
    case
        ( verticalOverlap claim1 claim2
        , horizontalOverlap claim1 claim2
        )
    of
        ( Just _, Just _ ) ->
            True

        ( _, _ ) ->
            False
