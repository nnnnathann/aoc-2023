app "aoc-2023"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        pf.Stdout,
        pf.Task,
        pf.Utc,
        pf.Arg,
        Advent.Day1.{ day1Solver },
        "input/day1.txt" as day1Input : Str,
        Advent.Day2.{ day2Solver },
        "input/day2.txt" as day2Input : Str,
    ]
    provides [main] to pf

days = [
    { num: 1, solver: day1Solver, input: day1Input },
    { num: 2, solver: day2Solver, input: day2Input },
]

Solver n1 n2 : { solve1 : Str -> Num n1, solve2 : Str -> Num n2 }

Day : { num : U8, solver : Solver, input : Str }

main : Task.Task {} I32
main =
    result <- Task.attempt readArgs
    when result is
        Ok (RunDay day) ->
            runDay day |> Task.mapErr (\_ -> 0)

        Err e ->
            Stdout.line (Inspect.toStr e)
            |> Task.mapErr (\_ -> 1)

Cmd : [
    RunDay Day,
]

readArgs : Task.Task Cmd [InvalidDayNumber, DayNotFound, NoArgument, TooManyArguments]
readArgs =
    args <- Arg.list |> Task.await
    when args is
        [_, numStr] ->
            Str.toU8 numStr
            |> Result.mapErr (\_ -> InvalidDayNumber)
            |> Result.try (\dayNum -> days |> List.findFirst (\{ num } -> num == dayNum) |> Result.mapErr (\_ -> DayNotFound))
            |> Task.fromResult
            |> Task.map RunDay

        [_] -> Task.err NoArgument
        _ -> Task.err TooManyArguments

runDay : Day -> Task.Task {} {}
runDay = \{ input, solver: { solve1, solve2 } } ->
    (one, oneMs) <- Task.await (bracketNanos (\_ -> solve1 input))
    (two, twoMs) <- Task.await (bracketNanos (\_ -> solve2 input))
    Stdout.line "Part 1: \(Inspect.toStr one) (\(Num.toStr oneMs)µs), Part 2: \(Inspect.toStr two) (\(Num.toStr twoMs)µs)"

bracketNanos : ({} -> a) -> Task.Task (a, U128) {}
bracketNanos = \f ->
    start <- Task.await Utc.now
    result = f {}
    end <- Task.await Utc.now
    Task.ok (result, Num.divTrunc (Utc.deltaAsNanos end start) 1000)
