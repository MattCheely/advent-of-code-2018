module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner as Runner exposing (BenchmarkProgram)
import Browser
import Data.Day1 as Day1
import Day1


main =
    Runner.program day1Bench


day1Bench : Benchmark
day1Bench =
    describe "Day 1"
        [ benchmark "Part 1" <|
            \_ -> Day1.part1 Day1.input
        , benchmark "Part 2" <|
            \_ -> Day1.part2 Day1.input
        ]
