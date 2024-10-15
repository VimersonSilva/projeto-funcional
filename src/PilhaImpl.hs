module PilhaImpl(
    Pilha(..),
    isEmpty,
    push,
    top
) where

data Pilha = Pilha [Integer] Integer Integer  -- Pilha Array/lista capacidade topo
    deriving Show

isEmpty:: Pilha -> Bool
isEmpty (Pilha lista capacidade topo) | topo == -1 = True
                      | otherwise = False

isFull:: Pilha -> Bool
isFull (Pilha lista capacidade topo) | capacidade == (topo - 1) = True
                                     | otherwise = False



push :: Pilha -> Integer -> Pilha
push (Pilha lista capacidade topo) x = Pilha ([x] ++ lista)

top :: Pilha -> Integer
top (Pilha lista) = head lista