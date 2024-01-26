#!/bin/sh

advent_session_id="${1:-ADVENT_SESSION_ID}"
year="${2:-2023}"

if [ -z "$advent_session_id" ]; then
    echo "Usage: $0 <advent_session_id> [year]"
    exit 1
fi

mkdir -p input

for f in {1..25}; do
    curl -H "Cookie: session=$advent_session_id" https://adventofcode.com/$year/day/$f/input >input/day$f.txt
done
