interface Advent.Day1
    exposes [day1Solver]
    imports [Advent.Input]

day1Solver = {
    solve1,
    solve2,
}

expect
    example1 =
        """
        1abc2xy
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
        """
    solve1 example1 == 142

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
    solve2 example2 == 281

solve1 = \input ->
    input
    |> Advent.Input.linesOfUtf8
    |> List.map (\line -> List.keepOks line Advent.Input.utf8Digit)
    |> List.map
        (\line ->
            when (List.first line, List.last line) is
                (Ok first, Ok last) -> first * 10 + last
                _ -> 0
        )
    |> List.sum

solve2 = \input ->
    Advent.Input.linesOfUtf8 input
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
