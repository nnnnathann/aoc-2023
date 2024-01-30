interface Advent.DictExtras
    exposes [insertAllWith, histogram]
    imports []

insertAllWith = \a, b, f ->
    Dict.walk
        a
        b
        (\carry, key, rightValue ->
            Dict.update
                carry
                key
                (\existing ->
                    when existing is
                        Missing -> Present rightValue
                        Present leftValue -> Present (f leftValue rightValue)
                )
        )

expect
    a = Dict.fromList [("a", 1), ("b", 2), ("c", 1)]
    b = Dict.fromList [("a", 3), ("c", 4)]
    expected = Dict.fromList [("a", 2), ("b", 2), ("c", 3)]
    result = insertAllWith a b (\left, right -> left - right)
    expected == result

histogram : List (a, U32) -> Dict a U32 where a implements Hash & Eq
histogram = \pairs ->
    pairs
    |> List.walk
        (Dict.empty {})
        (\carry, (key, value) ->
            Dict.update
                carry
                key
                (\existing ->
                    when existing is
                        Missing -> Present value
                        Present n -> Present (n + value)
                )
        )
