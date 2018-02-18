module Format (binShow, hexShow) where
    import Numeric
    import Data.Char
    hexShow, binShow :: (Num n, Integral n, Show n)  =>  Int -> n -> String
    hexShow digits n = pad digits '0' (showHex n "")


    binShow digits n = pad digits '0' $ showIntAtBase 2 (chr . (+ ord '0')) n ""

    pad digits padding = reverse . take digits . (++ repeat padding) . reverse