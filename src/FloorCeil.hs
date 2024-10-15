module FloorCeil(
    floorF,
    ceilF
) where

floorF :: Integer -> [Integer] -> Integer
floorF x (y:ys) = maxFunc (head lista) lista where
                 lista = [z | z <- (y:ys), x > z]

maxFunc :: Integer -> [Integer] -> Integer
maxFunc x [] = x
maxFunc x (y:ys) | x < y = maxFunc y ys
                 |otherwise = maxFunc x ys

ceilF :: Integer -> [Integer] -> Integer
ceilF x (y:ys) = minFunc (head lista) lista where
                 lista = [z | z <- (y:ys), x < z]

minFunc :: Integer -> [Integer] -> Integer
minFunc x [] = x
minFunc x (y:ys) | x > y = minFunc y ys
                 |otherwise = minFunc x ys