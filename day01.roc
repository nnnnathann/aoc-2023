app "day01"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br" }
    imports [
        "input/day1.txt" as inputFile : Str,
        pf.Stdout,
    ]
    provides [main] to pf

solution1 = \input -> sumFirstAndLastFromMap digits input

expect
    example1 =
        """
        1abc2xy
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
        """
    solution1 example1 == 142

solution2 = \input -> sumFirstAndLastFromMap allDigits input

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
    solution2 example2 == 281

sumFirstAndLastFromMap = \digitMap, str ->
    str
    |> lines
    |> List.map (\line -> firstLast digitMap line)
    |> List.sum

allDigits = List.concat digits wordDigits

digits = [
    ("0", 0),
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
]

wordDigits = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
]

tail : Str -> Str
tail = \str ->
    Str.graphemes str
    |> List.dropAt 0
    |> Str.joinWith ""

readDigit = \digitMap, str ->
    result = List.findFirst digitMap (\(digit, _) -> Str.startsWith str digit)
    when result is
        Ok (_, value) -> Ok (value, tail str)
        Err _ -> Err (tail str)

firstLast = \searchMap, inputStr ->
    gotDigit = \res, digit ->
        when res is
            Init -> Digits (digit, digit)
            Digits (f, _) -> Digits (f, digit)

    go = \carry, str ->
        if Str.isEmpty str then
            carry
        else
            when readDigit searchMap str is
                Ok (digit, rest) -> go (gotDigit carry digit) rest
                Err strRest -> go carry strRest

    when go Init inputStr is
        Digits (first, last) -> 10 * first + last
        Init -> 0

lines = \str -> Str.split str "\n"

main =
    one = solution1 inputFile
    two = solution2 inputFile
    Stdout.line "Part 1: \(Inspect.toStr one), Part 2: \(Inspect.toStr two)"

matchFirstInMap = \keywordMapUtf8, utf8Chars ->
    crash "not implemented"

matchLastInMap = \keywordMapUtf8, utf8Chars ->
    matchFirstInMap (getReveresedKeywords keywordMapUtf8) (List.reverse utf8Chars)

expect matchFirstInMap (getMapAsUtf8 [("1", 2), ("2", 1)]) (Str.toUtf8 "a11abc2xy") == Ok 2
expect matchLastInMap (getMapAsUtf8 [("1", 2), ("2", 1)]) (Str.toUtf8 "a11abc2xy") == Ok 1
expect matchFirstInMap (getMapAsUtf8 [("1", 1)]) (Str.toUtf8 "abc2xy") == Err NotFound

getReveresedKeywords = \keywordMapUtf8 ->
    List.map keywordMapUtf8 (\kv -> mapFirst kv List.reverse)

expect getReveresedKeywords [([49, 50], 1)] == [([50, 49], 1)]

getMapAsUtf8 = \keywordMap ->
    List.map keywordMap (\kv -> mapFirst kv Str.toUtf8)

expect getMapAsUtf8 [("12", 1)] == [([49, 50], 1)]

mapFirst = \(a, b), f ->
    (f a, b)

expect mapFirst (1, 2) (\x -> x + 1) == (2, 2)

startsWithAt = \str, index, prefix ->
    test = \i, elem ->
        when List.get prefix i is
            Ok c -> if c == elem then Continue (i + 1) else Break i
            Err _ -> Break i
    matchLen = List.walkFromUntil str index 0 test
    matchLen == List.len prefix

expect startsWithAt [0, 1, 2, 3, 4] 1 [1, 2, 3]
expect startsWithAt [0, 1, 2, 3, 4] 1 [1, 3] == Bool.false
