module PilhaImpl(
    Pilha(..),
    isEmpty,
    push,
    top,
    isFull,
    pop
) where

data Pilha = Pilha [Integer] Integer Integer  -- Pilha Array/lista capacidade topo
    deriving (Eq,Show)

isEmpty:: Pilha -> Bool
isEmpty (Pilha lista capacidade topo) | topo == -1 = True
                      | otherwise = False

isFull:: Pilha -> Bool
isFull (Pilha lista capacidade topo) | capacidade == (topo + 1) = True
                                     | otherwise = False

addLista :: [Integer] -> Integer -> Integer -> [Integer]
addLista (y:ys) topo x | topo > 0 = y : addLista ys (topo - 1) x
                       | otherwise = x : ys

push :: Pilha -> Integer -> Pilha
push (Pilha lista capacidade topo) x | isFull (Pilha lista capacidade topo) = error "Capacidade lotada!"
                                     | (fromIntegral (length lista)) == capacidade = Pilha (addLista lista (topo + 1) x) capacidade (topo + 1) 
                                     | otherwise = Pilha (lista ++ [x]) capacidade (topo + 1)

buscaTop :: [Integer] -> Integer -> Integer
buscaTop lista topo | topo > 0 = buscaTop (tail lista) (topo - 1)
                    | otherwise = head lista

top :: Pilha -> Integer
top (Pilha lista capacidade topo) = buscaTop lista topo

pop :: Pilha -> Pilha
pop (Pilha lista capacidade topo) | isEmpty (Pilha lista capacidade topo) = error "A pilha está vazia"
                                  | otherwise = (Pilha lista capacidade (topo - 1))