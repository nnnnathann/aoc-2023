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

Solver : { solve1 : Str -> I32, solve2 : Str -> I32 }

Day : { solver : Solver, input : Str }

main : Task.Task {} I32
main =
    day <- Task.await (runReport getDay)
    runDay day |> runReport

getDay : Task.Task Day [InvalidArgument, NoArgument]
getDay =
    args <- Task.await Arg.list
    when args is
        [_, "1"] -> Task.ok { solver: day1Solver, input: day1Input }
        [_, "2"] -> Task.ok { solver: day2Solver, input: day2Input }
        [] -> Task.err NoArgument
        _ -> Task.err InvalidArgument

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
    (one, oneMs) <- Task.await (bracketMillis (\_ -> solve1 input))
    (two, twoMs) <- Task.await (bracketMillis (\_ -> solve2 input))
    Stdout.line "Part 1: \(Inspect.toStr one) (\(Num.toStr oneMs)ms), Part 2: \(Inspect.toStr two) (\(Num.toStr twoMs)ms)"

bracketMillis : ({} -> a) -> Task.Task (a, U128) {}
bracketMillis = \f ->
    start <- Task.await Utc.now
    result = f {}
    end <- Task.await Utc.now
    Task.ok (result, Utc.deltaAsMillis end start)

