interface Advent.Input
    exposes [lines, linesOfChars, linesOfGraphemes, readDigits, digitsAsInt, split2]
    imports []

lines : Str -> List Str
lines = \str -> Str.split str "\n"

linesOfChars : Str -> List (List U8)
linesOfChars = \str -> lines str |> List.map Str.toUtf8

linesOfGraphemes : Str -> List (List Str)
linesOfGraphemes = \str -> lines str |> List.map Str.graphemes

digitsAsInt : Str -> I32
digitsAsInt = \str ->
    Str.graphemes str
    |> List.keepIf
        (\d ->
            when d is
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> Bool.true
                _ -> Bool.false
        )
    |> Str.joinWith ""
    |> Str.toI32
    |> Result.withDefault 0

readDigits : Str -> List U8
readDigits = \str ->
    Str.toUtf8 str
    |> List.keepOks readDigitChar

expect readDigits "alksdjf2345" == [2, 3, 4, 5]

readDigitChar : U8 -> Result U8 [NotADigit]
readDigitChar = \char ->
    when char is
        '0' -> Ok 0
        '1' -> Ok 1
        '2' -> Ok 2
        '3' -> Ok 3
        '4' -> Ok 4
        '5' -> Ok 5
        '6' -> Ok 6
        '7' -> Ok 7
        '8' -> Ok 8
        '9' -> Ok 9
        _ -> Err NotADigit

split2 : Str, Str -> Result (Str, Str) [InvalidSplit]
split2 = \str, sep ->
    when Str.split str sep is
        [a, b] -> Ok (a, b)
        _ -> Err InvalidSplit
