interface Advent.Day2
    exposes [day2Solver]
    imports [Advent.Input]

day2Solver = {
    solve1: \inputStr ->
        Advent.Input.lines inputStr
        |> List.keepOks (\line -> parseGame Init line)
        |> List.keepIf (\game -> isPossibleWith game (Dict.fromList [("red", 12), ("green", 13), ("blue", 14)]))
        |> List.map (\game -> game.number)
        |> List.sum,
    solve2: \_ -> 0,
}

isPossibleWith : Game, Dict Str U32 -> Bool
isPossibleWith = \game, total ->
    game.reveals
    |> List.dropIf (\reveal -> revealIsPossibleWith (Dict.fromList reveal) total)
    |> List.isEmpty

revealIsPossibleWith : Dict Str U32, Dict Str U32 -> Bool
revealIsPossibleWith = \reveal, totals ->
    reveal
    |> Dict.keepIf
        (\(color, count) ->
            when Dict.get totals color is
                Err KeyNotFound -> Bool.true
                Ok total -> count > total
        )
    |> Dict.isEmpty

gameTotals : Game -> List (Dict Str U32)
gameTotals = \game ->
    game.reveals
    |> List.map histogram

insertAllWith : Dict k v, Dict k v, (v, v -> v) -> Dict k v
insertAllWith = \a, b, f ->
    Dict.walk
        a
        b
        (\carry, key, rightValue ->
            Dict.update
                carry
                key
                (\existing ->
                    when existing is
                        Missing -> Present rightValue
                        Present leftValue -> Present (f leftValue rightValue)
                )
        )

expect
    a = Dict.fromList [("a", 1), ("b", 2), ("c", 1)]
    b = Dict.fromList [("a", 3), ("c", 4)]
    expected = Dict.fromList [("a", 2), ("b", 2), ("c", 3)]
    result = insertAllWith a b (\left, right -> left - right)
    expected == result

histogram : List (a, U32) -> Dict a U32
histogram = \pairs ->
    pairs
    |> List.walk
        (Dict.empty {})
        (\carry, (key, value) ->
            Dict.update
                carry
                key
                (\existing ->
                    when existing is
                        Missing -> Present value
                        Present n -> Present (n + value)
                )
        )

example1 =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
Game : { number : I32, reveals : List (List (Str, U32)) }

expect
    day2Solver.solve1 example1 == 8

ReadMode : [
    Init,
    GameNumber I32,
]
parseGame : ReadMode, Str -> Result Game [InvalidGameLine, NotEnoughReveals]
parseGame = \mode, str ->
    when mode is
        Init ->
            when Str.split str ": " is
                [gameStr, reveals] -> parseGame (GameNumber (Advent.Input.digitsAsInt gameStr)) reveals
                _ -> Err InvalidGameLine

        GameNumber gameNumber ->
            parseReveal = \reveal ->
                (countStr, colorStr) <- Advent.Input.split2 reveal " " |> Result.try
                when (Str.toU32 countStr, colorStr) is
                    (_, "") | (Err _, _) -> Err InvalidGameLine
                    (Ok n, color) -> Ok (color, n)
            parseReveals = \revealsStr ->
                revealsStr
                |> Str.split ", "
                |> List.keepOks parseReveal
            reveals = Str.split str "; " |> List.map parseReveals
            Ok { number: gameNumber, reveals: reveals }

expect
    result = parseGame Init "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    result == Ok { number: 1, reveals: [[("blue", 3), ("red", 4)], [("red", 1), ("green", 2), ("blue", 6)], [("green", 2)]] }

