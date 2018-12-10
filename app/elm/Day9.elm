module Day9 exposing (initialGame, initialGame2, part1)

-- Section: Input Parsing


initalPlayers =
    List.range 2 452
        |> List.map (\id -> Player id 0)


initialGame : Game
initialGame =
    { players = RingList [] (Player 1 0) initalPlayers
    , marbleState = RingList [] 0 []
    , nextMarble = 1
    , lastMarble = 71250
    }


initialGame2 : Game
initialGame2 =
    { players = RingList [] (Player 1 0) initalPlayers
    , marbleState = RingList [] 0 []
    , nextMarble = 1
    , lastMarble = 7125000
    }



-- Section: Part One


type RingList a
    = RingList (List a) a (List a)


goCounter : RingList a -> RingList a
goCounter ring =
    case ring of
        RingList [] current [] ->
            RingList [] current []

        RingList (newCurrent :: prev) current post ->
            RingList prev newCurrent (current :: post)

        RingList [] current post ->
            goCounter (RingList (List.reverse post) current [])


goClockwise : RingList a -> RingList a
goClockwise ring =
    case ring of
        RingList [] current [] ->
            RingList [] current []

        RingList prev current (newCurrent :: post) ->
            RingList (current :: prev) newCurrent post

        RingList prev current [] ->
            goClockwise (RingList [] current (List.reverse prev))


getCurrent : RingList a -> a
getCurrent ring =
    case ring of
        RingList _ a _ ->
            a


setCurrent : a -> RingList a -> RingList a
setCurrent newVal ring =
    case ring of
        RingList prev _ post ->
            RingList prev newVal post


insertAfter : a -> RingList a -> RingList a
insertAfter val ring =
    case ring of
        RingList prev cur post ->
            RingList (cur :: prev) val post


removeCurrent : RingList a -> ( a, RingList a )
removeCurrent ring =
    case ring of
        RingList [] cur [] ->
            ( cur, ring )

        RingList prev cur (newCur :: post) ->
            ( cur, RingList prev newCur post )

        RingList pre cur [] ->
            removeCurrent (RingList [] cur (List.reverse pre))


type alias Player =
    { id : Int, score : Int }


type alias Game =
    { players : RingList Player
    , marbleState : RingList Int
    , nextMarble : Int
    , lastMarble : Int
    }


part1 : Game -> Int
part1 game =
    let
        newGame =
            advanceGame game
    in
    if newGame.nextMarble > newGame.lastMarble then
        getHighScore newGame.players

    else
        part1 newGame


advanceGame : Game -> Game
advanceGame game =
    if modBy 23 game.nextMarble == 0 then
        doWeirdRule game

    else
        doRegularRule game


doWeirdRule : Game -> Game
doWeirdRule game =
    let
        player =
            getCurrent game.players

        ( removed, newMarbles ) =
            game.marbleState
                |> do 7 goCounter
                |> removeCurrent
    in
    { players =
        game.players
            |> setCurrent { player | score = player.score + removed + game.nextMarble }
            |> goClockwise
    , marbleState = newMarbles
    , nextMarble = game.nextMarble + 1
    , lastMarble = game.lastMarble
    }


doRegularRule : Game -> Game
doRegularRule game =
    { players = goClockwise game.players
    , marbleState =
        goClockwise game.marbleState
            |> insertAfter game.nextMarble
    , nextMarble = game.nextMarble + 1
    , lastMarble = game.lastMarble
    }


getHighScore : RingList Player -> Int
getHighScore players =
    case players of
        RingList prev player post ->
            player
                :: post
                ++ prev
                |> List.map .score
                |> List.maximum
                |> Maybe.withDefault 0


do : Int -> (a -> a) -> a -> a
do times fn input =
    List.repeat times Nothing
        |> List.foldl
            (\_ acc -> fn acc)
            input



-- Section: Part Two


part2 : Game -> Int
part2 =
    part1
