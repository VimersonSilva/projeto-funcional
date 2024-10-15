module Student (
    Aluno(..),
    mediaCRA,
    groupByCRA
) where

data Aluno = Aluno Integer String String String Float
              deriving (Eq,Show)

mediaCRA :: [Aluno] -> Float
mediaCRA [] = error "Não há alunos!"
mediaCRA lista = somaFoldr / fromIntegral (length lista) where
                            somaFoldr = foldr (+) 0 num
                            num = [cra | Aluno _ _ _ _ cra <- lista]

groupByCRA :: [Aluno] -> Float -> (Float, [Aluno])
groupByCRA lista x = (x, sameCRA) where
                   sameCRA = [(Aluno mat name ln periodo cra) | (Aluno mat name ln periodo cra) <- lista, cra == x]

