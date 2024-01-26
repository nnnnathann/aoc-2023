app "bin/day1"
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

sumFirstAndLastFromMap = \stringDigitMap, inputStr ->
    listDigitMap = getMapAsUtf8 stringDigitMap
    lines inputStr 
    |> List.map Str.toUtf8
    |> List.map (\line -> getFirstAndLastAsNum line listDigitMap)
    |> List.sum

expect 
    result = sumFirstAndLastFromMap [("1", 1), ("one", 1), ("eight", 8)] "aa12oneight\noneaaaa"
    result == 18 + 11

getFirstAndLastAsNum = \chars, searchMap ->
    when (matchFirstInMap searchMap chars, matchLastInMap searchMap chars) is
        (Ok first, Ok last) -> 10 * first + last
        _ -> 0
expect
    result = getFirstAndLastAsNum (Str.toUtf8 "aa12two") (getMapAsUtf8 [("1", 1), ("two", 2)])
    result == 12

allDigitsDict = Dict.insertAll digitsDict wordDigitsDict

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
digitsDict = Dict.fromList (getMapAsUtf8 digits)

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
wordDigitsDict = Dict.fromList (getMapAsUtf8 wordDigits)

lines = \str -> Str.split str "\n"

main =
    one = solution1 inputFile
    two = solution2 inputFile
    Stdout.line "Part 1: \(Inspect.toStr one), Part 2: \(Inspect.toStr two)"

# given a list of (List elem, value) pairs,
# find the first pair whose (List elem) is a
# sublist of the given input, and return the
# value of that pair
matchFirstInMap = \keywordMapUtf8, utf8Chars ->
    go = \seq ->
        if seq == [] then
            Err NotFound
        else
            foundSublist = List.findFirst keywordMapUtf8 (\(keyword, _) -> List.startsWith seq keyword)
            when foundSublist is
                Ok (_, value) -> Ok value
                Err _ -> go (List.dropFirst seq 1)
    go utf8Chars
expect matchFirstInMap (getMapAsUtf8 [("1", 2), ("2", 1)]) (Str.toUtf8 "a11abc2xy") == Ok 2
expect matchFirstInMap (getMapAsUtf8 [("1", 1)]) (Str.toUtf8 "abc2xy") == Err NotFound

matchLastInMap = \keywordMapUtf8, utf8Chars ->
    matchFirstInMap (getReversedKeywords keywordMapUtf8) (List.reverse utf8Chars)
expect matchLastInMap (getMapAsUtf8 [("1", 2), ("2", 1)]) (Str.toUtf8 "a11abc2xy") == Ok 1

getReversedKeywords = \keywordMapUtf8 ->
    List.map keywordMapUtf8 (\kv -> mapFirst kv List.reverse)
expect getReversedKeywords [([49, 50], 1)] == [([50, 49], 1)]

getMapAsUtf8 = \keywordMap ->
    List.map keywordMap (\kv -> mapFirst kv Str.toUtf8)
expect getMapAsUtf8 [("12", 1)] == [([49, 50], 1)]

mapFirst = \(a, b), f ->
    (f a, b)
expect mapFirst (1, 2) (\x -> x + 1) == (2, 2)
