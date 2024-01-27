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
    (one, oneMs) <- Task.await (bracketMillis (\_ -> solution1 inputData))
    (two, twoMs) <- Task.await (bracketMillis (\_ -> solution2 inputData))
    Stdout.line "Part 1: \(Inspect.toStr one) (\(Num.toStr oneMs)ms), Part 2: \(Inspect.toStr two) (\(Num.toStr twoMs)ms)"

bracketMillis : ({} -> a) -> Task.Task (a, U128) {}
bracketMillis = \f ->
    start <- Task.await Utc.now
    result = f {}
    end <- Task.await Utc.now
    Task.ok (result, Utc.deltaAsMillis end start)

solution1 = \input -> solveDigitPuzzle input digitsDict

expect
    example1 =
        """
        1abc2xy
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
        """
    solution1 example1 == 142

solution2 = \input -> solveDigitPuzzle input allDigitsDict

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

solveDigitPuzzle = \inputStr, digitLookup ->
    linesOfChars inputStr
    |> List.map (\line -> getFirstAndLastAsNum line digitLookup)
    |> List.sum

linesOfChars = \str -> lines str |> List.map Str.toUtf8

# accepts a haystack, and a dict of needles to values,
# and returns the first and last sublist of haystack that
# matches a key of the dict, and if at least one exists,
# returns the values of the first and last matching key
getFirstAndLastSublist : List elem, Dict (List elem) value -> Result (value, value) [NotFound] where elem implements Hash & Eq
getFirstAndLastSublist = \haystack, lookup ->
    Dict.keys lookup
    |> (\needles ->
        when (findFirstSublist haystack needles, findLastSublist haystack needles) is
            (Ok first, Ok last) ->
                when (Dict.get lookup first, Dict.get lookup last) is
                    (Ok firstValue, Ok lastValue) -> Ok (firstValue, lastValue)
                    _ -> Err NotFound

            _ -> Err NotFound
    )
expect getFirstAndLastSublist [0, 1, 2, 3, 4] (Dict.fromList [([1, 2], 1), ([2, 3], 2)]) == Ok (1, 2)
expect getFirstAndLastSublist (Str.toUtf8 "aa123aaef9one") digitsDict == Ok (1, 9)
expect getFirstAndLastSublist (Str.toUtf8 "aa123aaefoneight") allDigitsDict == Ok (1, 8)

expect
    result = solveDigitPuzzle "aa12oneight\noneaaaa" allDigitsDict
    result == 18 + 11

getFirstAndLastAsNum = \chars, searchMap ->
    when getFirstAndLastSublist chars searchMap is
        Ok (first, last) -> Num.toU32 (first * 10 + last)
        _ -> 0
expect
    result = getFirstAndLastAsNum (Str.toUtf8 "aa12two") allDigitsDict
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

getReversedKeywords = \keywordMapUtf8 ->
    List.map keywordMapUtf8 (\kv -> mapFirst kv List.reverse)
expect getReversedKeywords [([49, 50], 1)] == [([50, 49], 1)]

getMapAsUtf8 = \keywordMap ->
    List.map keywordMap (\kv -> mapFirst kv Str.toUtf8)
expect getMapAsUtf8 [("12", 1)] == [([49, 50], 1)]

mapFirst = \(a, b), f ->
    (f a, b)
expect mapFirst (1, 2) (\x -> x + 1) == (2, 2)

startsWithAt = \haystack, needle, index ->
    List.sublist haystack { start: index, len: List.len needle } == needle
expect startsWithAt [0, 1, 2, 3, 4] [1, 2, 3] 1
expect startsWithAt [0, 1, 2, 3, 4] [1, 3] 1 == Bool.false

findLastSublist = \haystack, needles ->
    findFirstSublist (List.reverse haystack) (List.map needles List.reverse)
    |> Result.map List.reverse
expect findLastSublist [0, 1, 2, 3, 4] [[1, 2], [2, 3]] == Ok [2, 3]

findFirstSublist = \haystack, needles ->
    checkIndex = \state, _, index ->
        when findFirstStartsWithAt haystack needles index is
            Ok needle -> Break (Ok needle)
            Err _ -> Continue state
    List.walkWithIndexUntil haystack (Err NotFound) checkIndex
expect findFirstSublist [0, 1, 2, 3, 4] [[1, 2, 3], [2, 3]] == Ok [1, 2, 3]

findFirstStartsWithAt = \haystack, needles, index ->
    List.findFirst needles (\needle -> startsWithAt haystack needle index)
expect findFirstStartsWithAt [0, 1, 2, 3, 4] [[1, 2, 3], [2, 3]] 1 == Ok [1, 2, 3]

