"""
trebuchet: finding calibration values via filtering and parsing
"""

import advent

def solve(input):
    """
    >>> solve("1abc2\\npqr3stu8vwx\\na1b2c3d4e5f\\ntreb7uchet")
    142
    142

    >>> solve("two1nine\\neightwothree\\nabcone2threexyz\\nxtwone3four\\n4nineeightseven2\\nzoneight234\\n7pqrstsixteen")
    209
    281
    """
    inputlines = input.splitlines()
    number_mapping = {"1": "1", "2": "2", "3": "3", "4": "4", "5": "5",
                  "6": "6", "7": "7", "8": "8", "9": "9"}
    part1 = sum([calibration_value(line, number_mapping) for line in inputlines])
    print(part1)
    number_mapping_2 = {**number_mapping, "one": "1", "two": "2", "three": "3", "four": "4", "five": "5",
                "six": "6", "seven": "7", "eight": "8", "nine": "9"}
    part2 = sum([calibration_value(line, number_mapping_2) for line in inputlines])
    print(part2)

def calibration_value(line, mapping):
    """
    >>> calibration_value("1abc2", {"1": 1, "2": 2})
    12
    >>> calibration_value("pqr3stu8twowx", {"3": 3, "8": 8, "two": 2})
    32
    """
    match1 = first_match(line, mapping.keys())
    match2 = last_match(line, mapping.keys())
    if match1 == None or match2 == None:
        return 0
    return int(str(mapping[match1]) + "" + str(mapping[match2]))

def first_match(line, items):
   """
   match the first of a set of items in a string
   >>> first_match("1abc2", ["c2", "abc", "2"])
   'abc'
   """
   min_index = None
   min_match = None
   for key in items:
       if key in line and (min_index == None or line.index(key) <= min_index):
           min_index = line.index(key)
           min_match = key
   return min_match

def last_match(line, mapping):
    """
    match the last of a set of items in a string
    >>> last_match("1abc2", ["c2", "abc", "2"])
    '2'
    """
    match = first_match(line [::-1], [x [::-1] for x in mapping])
    if match == None:
        return None
    return match[::-1]

if __name__ == "__main__":
    solve(advent.read_or_download(2023, 1))
    import doctest
    doctest.testmod()