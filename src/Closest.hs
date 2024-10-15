module Closest (
    closest
) where

closest :: [Integer] -> Integer -> (Integer, Integer)
closest (y:ys) x = calcClosest lista x (head lista) where
                 lista = [(x,y) | x <- (y:ys), y <- (y:ys), x /= y]

calcClosest :: [(Integer, Integer)] -> Integer -> (Integer, Integer) -> (Integer, Integer)
calcClosest [] x z = z
calcClosest (y:ys) x z | abs ((fst y + snd y) - x) < abs ((fst z + snd z) - x) = calcClosest ys x y
                       | otherwise = calcClosest ys x z