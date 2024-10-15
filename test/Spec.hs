import Test.HUnit
import Student
import Closest
import FloorCeil
import Control.Exception (try, evaluate, SomeException)

test1 :: Test
test1 = TestCase (assertEqual "Media de um unico aluno" 7.0 (mediaCRA [Aluno 12 "AB" "CD" "24.1" 7.0]))

test2 :: Test
test2 = TestCase (assertEqual "fazer media dos CRAs 7 e 6.5" 6.75 (mediaCRA [(Aluno 12 "AB" "CD" "24.1" 7.0), (Aluno 12 "AB" "CD" "24.1" 6.5)]))

test3 :: Test
test3 = TestCase (assertEqual "fazer media dos CRAs em caso limite" 10.0 (mediaCRA [(Aluno 12 "AB" "CD" "24.1" 10.0), (Aluno 12 "AB" "CD" "24.1" 10.0)]))

test4 :: Test
test4 = TestCase (assertEqual "fazer media dos CRAs em caso limite" 0.0 (mediaCRA [(Aluno 12 "AB" "CD" "24.1" 0.0)]))

testSemAlunos :: Test
testSemAlunos = TestCase $ do
    result <- try (evaluate (mediaCRA [])) :: IO (Either SomeException Float)
    case result of
      Left ex  -> return ()  -- A exceção foi capturada, o teste passa
      Right _  -> assertFailure "Esperava uma exceção, mas não ocorreu."

--testes  groupBy

testGroup :: Test
testGroup = TestCase (assertEqual "GroupBy dos CRAs" (10.0, [(Aluno 12 "AB" "CD" "24.1" 10.0)]) (groupByCRA [(Aluno 12 "AB" "CD" "24.1" 10.0), (Aluno 12 "joao" "CD" "24.1" 0.0)] 10.0))

testGroup1 :: Test
testGroup1 = TestCase (assertEqual "GroupBy dos CRAs sem ninguem com o CRA" (9.0, []) (groupByCRA [(Aluno 12 "AB" "CD" "24.1" 10.0), (Aluno 12 "joao" "CD" "24.1" 0.0)] 9.0))

testGroup2 :: Test
testGroup2 = TestCase (assertEqual "GroupBy dos CRAs com retorno mais de um Aluno" (10.0, [(Aluno 12 "Joao" "CD" "24.1" 10.0),(Aluno 12 "AB" "CD" "24.1" 10.0)]) (groupByCRA [(Aluno 12 "Joao" "CD" "24.1" 10.0), (Aluno 12 "AB" "CD" "24.1" 10.0)] 10.0))

-- testes Questao 1 : somar dois numeros que se aproximem de o maximo de x

testClosest :: Test
testClosest = TestCase (assertEqual "Teste Closest de um array" (22, 30) (closest [10, 22, 28, 29, 30, 40] 54))

testClosest2 :: Test
testClosest2 = TestCase (assertEqual "Teste Closest de um array caso 2" (4, 10) (closest [1, 1, 3, 4, 7, 10] 15))

testClosest3 :: Test
testClosest3 = TestCase (assertEqual "Teste Closest de um array caso 3 com valores de soma exatos" (4, 10) (closest [1, 3, 4, 7, 10] 14))

-- testes Questao 2 - Floor e Ceil

testFloor :: Test
testFloor = TestCase (assertEqual "Teste Floor valor fora do escopo array" 10 (floorF 15 [1, 3, 4, 7, 10]))

testFloor1 :: Test
testFloor1 = TestCase (assertEqual "Teste Floor valor dentro do array" 4 (floorF 5 [1, 3, 4, 7, 10]))

testCeil :: Test
testCeil = TestCase (assertEqual "Teste Ceil valor dentro do array" 7 (ceilF 5 [1, 3, 4, 7, 10]))

testCeil1 :: Test
testCeil1 = TestCase (assertEqual "Teste Ceil valor fora do escopo do array" 1 (ceilF 0 [1, 3, 4, 7, 10]))

tests :: Test
tests = TestList [TestLabel "Teste MediaCRA" test1,
                  TestLabel "Teste MediaCRA" test2,
                  TestLabel "Teste MediaCRA" test3,
                  TestLabel "Teste MediaCRA" testSemAlunos,
                  TestLabel "Teste GroupBy" testGroup,
                  TestLabel "Teste GroupBy" testGroup1,
                  TestLabel "Teste GroupBy" testGroup2,
                  TestLabel "Teste GroupBy" testClosest,
                  TestLabel "Teste GroupBy" testClosest2,
                  TestLabel "Teste GroupBy" testClosest3,
                  TestLabel "Teste GroupBy" testFloor,
                  TestLabel "Teste GroupBy" testFloor1,
                  TestLabel "Teste GroupBy" testCeil,
                  TestLabel "Teste GroupBy" testCeil1]

main :: IO Counts
main = runTestTT tests


