module Day10 (d10) where 
import Advent2017
import Knot
import Format
lengths = [230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167]

d10 = hash lengths 256

d10p2 = do
    t <- dataFile "day10.txt"
    print . concatMap (hexShow 2) . knot . init $ t


