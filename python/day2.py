"""
cube conundrum
"""

import advent
import math

def solve(input):
    """
    >>> solve("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
    8
    2286
    """
    games = [parse_game(line) for line in input.splitlines() if ":" in line]
    configuration = {"red": 12, "green": 13, "blue": 14}
    possible_game_ids = [id for id, pulls in games if game_is_possible(pulls, configuration)]
    print(sum(possible_game_ids))
    game_powers = [game_power(pulls) for id, pulls in games]
    print(sum(game_powers))

def game_is_possible(game, configuration):
    for reveal in game:
        remaining = configuration.copy()
        for amt, color in reveal:
            if remaining[color] < amt:
                return False
            remaining[color] -= amt
    return True

def game_power(game):
    """
    >>> game_power([[[3, 'blue'], [4, 'red']], [[1, 'red'], [2, 'green'], [6, 'blue']], [[2, 'green']]])
    48
    """
    max_by_color = {}
    for reveal in game:
        for amt, color in reveal:
            if color not in max_by_color or amt > max_by_color[color]:
                max_by_color[color] = amt
    return math.prod(max_by_color.values())


def parse_game(game_line):
    """
    >>> parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    (1, [[[3, 'blue'], [4, 'red']], [[1, 'red'], [2, 'green'], [6, 'blue']], [[2, 'green']]])
    """
    [game_id, game] = game_line.split(":")
    game_id = int(game_id.split()[1])
    reveals = game.strip().split(";")
    pulls = [[pull.strip().split() for pull in reveal.split(",")] for reveal in reveals]
    pulls = [[[int(pull[0]), pull[1]] for pull in reveal] for reveal in pulls]
    return game_id, pulls

if __name__ == "__main__":
    solve(advent.read_or_download(2023, 2))
    import doctest
    doctest.testmod()
