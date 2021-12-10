# Advent-of-Code
[Advent of Code](https://adventofcode.com/) submissions

```
for i in $(seq -f '%02g' 1 25); do mkdir -p y2021/src/main/scala/nl/kevinvandervlist/aoc2021/day${i}; done
for i in $(seq -f '%02g' 1 25); do mkdir -p y2021/src/test/scala/nl/kevinvandervlist/aoc2021/day${i}; done
for i in $(seq -f '%02g' 1 25); do touch y2021/src/main/resources/day-${i}-input; done
for i in $(seq -f '%02g' 2 25); do cp y2021/src/test/scala/nl/kevinvandervlist/aoc2021/day01/XSpec.scala  y2021/src/test/scala/nl/kevinvandervlist/aoc2021/day${i}/XSpec.scala; done
```
