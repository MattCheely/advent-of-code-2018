module Helpers exposing (ListOrDup(..), Tree(..), insertIfUnique)


insertIfUnique : Int -> Tree -> ListOrDup Tree
insertIfUnique inbound tree =
    doInsertIfUniqueTree inbound tree


doInsertIfUniqueList : Int -> List Int -> List Int -> ListOrDup (List Int)
doInsertIfUniqueList inbound lower maybeHigher =
    case maybeHigher of
        comp :: rest ->
            case compare inbound comp of
                EQ ->
                    Duplicate

                GT ->
                    doInsertIfUniqueList inbound (comp :: lower) rest

                LT ->
                    Inserted (List.reverse (inbound :: lower) ++ (comp :: rest))

        [] ->
            Inserted (List.reverse (inbound :: lower))


doInsertIfUniqueTree : Int -> Tree -> ListOrDup Tree
doInsertIfUniqueTree inbound tree =
    case tree of
        Branch val left right ->
            case compare inbound val of
                LT ->
                    let
                        newLeft =
                            doInsertIfUniqueTree inbound left
                    in
                    case newLeft of
                        Inserted leftTree ->
                            Inserted (Branch val leftTree right)

                        Duplicate ->
                            Duplicate

                GT ->
                    let
                        newRight =
                            doInsertIfUniqueTree inbound right
                    in
                    case newRight of
                        Inserted rightTree ->
                            Inserted (Branch val left rightTree)

                        Duplicate ->
                            Duplicate

                EQ ->
                    Duplicate

        Empty ->
            Inserted (Branch inbound Empty Empty)


type Tree
    = Branch Int Tree Tree
    | Empty


type ListOrDup a
    = Inserted a
    | Duplicate
