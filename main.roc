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

Solver : { solve1 : Str -> I32, solve2 : Str -> I32 }

Day : { num : U8, solver : Solver, input : Str }

main : Task.Task {} I32
main =
    day <- Task.await (getDay |> runReport)
    runDay day |> runReport

getDayRunner : Str -> Result Day [InvalidDayNumber, DayNotFound]
getDayRunner = \str ->
    Str.toU8 str
    |> Result.mapErr (\_ -> InvalidDayNumber)
    |> Result.try (\dayNum -> days |> List.findFirst (\{ num } -> num == dayNum) |> Result.mapErr (\_ -> DayNotFound))

getDay : Task.Task Day [InvalidDayNumber, DayNotFound, NoArgument, TooManyArguments]
getDay =
    args <- Arg.list |> Task.await
    when args is
        [_, num] ->
            getDayRunner num |> Task.fromResult

        [_] -> Task.err NoArgument
        _ -> Task.err TooManyArguments

runReport : Task.Task a e -> Task.Task a I32 where e implements Inspect
runReport = \task ->
    result <- Task.attempt task
    when result is
        Ok x -> Task.ok x
        Err e ->
            _ <- Task.await (Stdout.line (Inspect.toStr e))
            Task.err 1

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
