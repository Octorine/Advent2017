module Advent2017 where

dataFile :: String -> IO String
dataFile s = readFile $ "/home/james/Code/advent/2017/data/" ++ s

