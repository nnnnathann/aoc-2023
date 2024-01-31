interface Advent.Input
    exposes [lines, linesOfUtf8, readDigits, digitsAsInt, split2, utf8Digit]
    imports []

lines : Str -> List Str
lines = \str -> Str.split str "\n"

linesOfUtf8 : Str -> List (List U8)
linesOfUtf8 = \str -> lines str |> List.map Str.toUtf8

digitsAsInt : Str -> U128
digitsAsInt = \str ->
    str
    |> Str.walkUtf8 [] (\prev, c -> List.appendIfOk prev (utf8Digit c))
    |> List.walk 0 (\acc, d -> acc * 10 + (Num.toU128 d))

expect digitsAsInt "1a23j" == 123

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

utf8Digit : U8 -> Result U8 [NotADigit]
utf8Digit = \code ->
    if code < 48 || code > 57 then
        Err NotADigit
    else
        Ok (code - 48)
