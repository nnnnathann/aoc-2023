app "bin/day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        "input/day1.txt" as inputFile : Str,
        pf.Stdout,
        pf.Task,
        pf.Utc,
    ]
    provides [main] to pf

main : Task.Task {} I32
main =
    runSolver inputFile
    |> Task.mapErr (\_ -> 1)

runSolver : Str -> Task.Task {} {}
runSolver = \inputData ->
    (one, oneMs) <- Task.await (bracketMillis (\_ -> solution1b inputData))
    (two, twoMs) <- Task.await (bracketMillis (\_ -> solution2b inputData))
    Stdout.line "Part 1: \(Inspect.toStr one) (\(Num.toStr oneMs)ms), Part 2: \(Inspect.toStr two) (\(Num.toStr twoMs)ms)"

bracketMillis : ({} -> a) -> Task.Task (a, U128) {}
bracketMillis = \f ->
    start <- Task.await Utc.now
    result = f {}
    end <- Task.await Utc.now
    Task.ok (result, Utc.deltaAsMillis end start)

expect
    example1 =
        """
        1abc2xy
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
        """
    solution1b example1 == 142

expect
    example2 =
        """
        two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen
        """
    solution2b example2 == 281

lines = \str -> Str.split str "\n"

linesOfChars = \str -> lines str |> List.map Str.toUtf8

solution1b = \input ->
    lines input
    |> List.map Str.graphemes
    |> List.map (\line -> List.keepOks line Str.toI32)
    |> List.map
        (\line ->
            when (List.first line, List.last line) is
                (Ok first, Ok last) -> first * 10 + last
                _ -> 0
        )
    |> List.sum

solution2b = \input ->
    linesOfChars input
    |> List.map (\line -> (getFirstDigit line, getLastDigit line))
    |> List.map (\(first, last) -> first * 10 + last)
    |> List.sum

getFirstDigit = \str ->
    when str is
        ['1', ..] -> 1
        ['2', ..] -> 2
        ['3', ..] -> 3
        ['4', ..] -> 4
        ['5', ..] -> 5
        ['6', ..] -> 6
        ['7', ..] -> 7
        ['8', ..] -> 8
        ['9', ..] -> 9
        ['o', 'n', 'e', ..] -> 1
        ['t', 'w', 'o', ..] -> 2
        ['t', 'h', 'r', 'e', 'e', ..] -> 3
        ['f', 'o', 'u', 'r', ..] -> 4
        ['f', 'i', 'v', 'e', ..] -> 5
        ['s', 'i', 'x', ..] -> 6
        ['s', 'e', 'v', 'e', 'n', ..] -> 7
        ['e', 'i', 'g', 'h', 't', ..] -> 8
        ['n', 'i', 'n', 'e', ..] -> 9
        [] -> 0
        _ -> getFirstDigit (List.dropFirst str 1)

expect getFirstDigit ("123" |> Str.toUtf8) == 1
expect getFirstDigit ("aaab23" |> Str.toUtf8) == 2

getLastDigit = \str ->
    when str is
        [.., '1'] -> 1
        [.., '2'] -> 2
        [.., '3'] -> 3
        [.., '4'] -> 4
        [.., '5'] -> 5
        [.., '6'] -> 6
        [.., '7'] -> 7
        [.., '8'] -> 8
        [.., '9'] -> 9
        [.., 'o', 'n', 'e'] -> 1
        [.., 't', 'w', 'o'] -> 2
        [.., 't', 'h', 'r', 'e', 'e'] -> 3
        [.., 'f', 'o', 'u', 'r'] -> 4
        [.., 'f', 'i', 'v', 'e'] -> 5
        [.., 's', 'i', 'x'] -> 6
        [.., 's', 'e', 'v', 'e', 'n'] -> 7
        [.., 'e', 'i', 'g', 'h', 't'] -> 8
        [.., 'n', 'i', 'n', 'e'] -> 9
        [] -> 0
        _ -> getLastDigit (List.dropLast str 1)
