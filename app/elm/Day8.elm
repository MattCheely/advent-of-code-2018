module Day8 exposing (part1, part2, tree)

import Data.Day8 exposing (inputStr, testStr)
import Dict exposing (Dict)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , int
        , loop
        , spaces
        , succeed
        )



-- Section: Input Paring


type alias Node =
    { children : Children
    , metadata : List Int
    }


type Children
    = Children (List Node)


type alias NodeHeader =
    { childCount : Int
    , metaDataCount : Int
    }


tree : Node
tree =
    Parser.run nodeParser inputStr
        |> Result.withDefault
            { children = Children []
            , metadata = []
            }


nodeParser : Parser Node
nodeParser =
    succeed identity
        |. spaces
        |= headerParser
        |> Parser.andThen
            (\header ->
                succeed Node
                    |. spaces
                    |= childrenParser header.childCount
                    |. spaces
                    |= metaDataParser header.metaDataCount
            )


headerParser : Parser NodeHeader
headerParser =
    succeed NodeHeader
        |. spaces
        |= int
        |. spaces
        |= int


childrenParser : Int -> Parser Children
childrenParser count =
    parseN count nodeParser
        |> Parser.map Children


metaDataParser : Int -> Parser (List Int)
metaDataParser count =
    parseN count
        (succeed identity
            |. spaces
            |= int
        )


parseN : Int -> Parser a -> Parser (List a)
parseN total parser =
    loop ( 0, [] )
        (\( count, collected ) ->
            if count == total then
                succeed ()
                    |> Parser.map (finish collected)

            else
                succeed (getMore count collected)
                    |= parser
        )


finish : List a -> () -> Step ( Int, List a ) (List a)
finish collected _ =
    Done (List.reverse collected)


getMore : Int -> List a -> a -> Step ( Int, List a ) (List a)
getMore count collected parsed =
    Loop ( count + 1, parsed :: collected )



-- Section: Part One


part1 : Node -> Int
part1 root =
    case root.children of
        Children nodeList ->
            List.sum root.metadata + List.sum (List.map part1 nodeList)



-- Section: Part Two


part2 : Node -> Int
part2 node =
    getNodeValue node


getNodeValue : Node -> Int
getNodeValue node =
    case node.children of
        Children nodeList ->
            case nodeList of
                [] ->
                    List.sum node.metadata

                _ ->
                    let
                        multipliers =
                            getNodeMultipliers node.metadata
                    in
                    List.indexedMap
                        (getMultipliedValue multipliers)
                        nodeList
                        |> List.sum


getMultipliedValue : Dict Int Int -> Int -> Node -> Int
getMultipliedValue multipliers idx node =
    case Dict.get (idx + 1) multipliers of
        Nothing ->
            0

        Just multiplier ->
            multiplier * getNodeValue node


getNodeMultipliers : List Int -> Dict Int Int
getNodeMultipliers metadata =
    List.foldl
        (\meta ->
            Dict.update meta (Just << (Maybe.withDefault 0 >> (+) 1))
        )
        Dict.empty
        metadata
