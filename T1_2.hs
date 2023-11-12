{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o segundo trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: intervalos, listas em compreensão, tuplos.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_02 where
import Data.Char

{-

8. Determine o valor de cada expressão:

a) [2*x | x <- [1,2,3]]
-}

ex8a = [2,4,6]


{-
b) [x^2 | x <- [1..8], x `mod` 2 == 0]
-}

ex8b = [4,16,36,64]

{-
c) [x | x <- ['6'..'S'], isDigit x]
-}

ex8c = "6789"

{-
d) [(x,y)| x <- [1..3], odd x, y <- [1..3]]
-}

ex8d = [(1,1), (1,2), (1,3), (3,1), (3,2), (3,3)]

{-
e) [(x,y)| x <- [1..3], y <- [1..3], odd x]
-}

ex8e = [(1,1), (1,2), (1,3), (3,1), (3,2), (3,3)]

{-

9. Utilizando uma lista em compreensão escreva uma expressão que calcule a soma
1^2 + 2^2 + ... + 100^2 dos quadrados dos primeiros 100 inteiros.

-}

somaQuadradosPrimeirosInteiros :: Int
somaQuadradosPrimeirosInteiros = sum [x^2 |x <- [1..100]]


{-
10. Escreva uma função que devolva quantos números entre 1 e n têm o seu
quadrado par.
-}

contaQuadradosPares :: Int -> Int
contaQuadradosPares n = length [x | x <- [1..n], x^2 `mod` 2 == 0]


{-
11. Dizemos que triplo (x,y,z) é Pitagórico se x^2 + y^2 = z^2. Utilizando
uma lista em compreensão defina a função pitagoricos que calcule a lista de
todos os triplos pitagóricos até um dado limite. Evite colocar triplos que
representem o mesmo triângulo, por exemplo (3,4,5) e (4,3,5).

ghci> pitagoricos 10
[(3,4,5),(6,8,10)]
-}

pitagoricos :: Int -> [(Int,Int,Int)]
pitagoricos = undefined

{-
12. Dizemos que um inteiro positivo é perfeito se é igual à soma de todos os
seus fatores, excluindo o próprio número.

(a) Utilizando uma lista em compreensão escreva a função fatores, que devolve os
fatores do inteiro dado (por uma qualquer ordem).
-}

fatores :: Int -> [Int]
fatores n = [x | x<- [1..n], n `mod` x == 0]

{-

(b) Defina agora uma função perfeitos que calcula a lista de todos os números
perfeitos até um dado limite.

-}

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], sum (fatores x) == 2 * x]

{-
13. Defina a lista infinita com todas as potências de dois.
-}

potenciasDeDoisInfinita :: [Int]
potenciasDeDoisInfinita = [2 ^ x | x <- [0..]]

{-
14. Defina a função produtoEscalar que calcula o produto escalar de dois
vetores.

Assuma que cada vetor é representado por uma lista e que as duas listas
têm o mesmo comprimento.
-}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar xs ys = sum [fst x * snd x | x <- zip xs ys]

{-
17. Defina a função pares de modo a que pares n seja a lista composta por todos
os valores (i, j) com 1 ≤ i ≤ n, 1 ≤ j ≤ n que satisfaçam a condição i ∕= j.
-}

pares :: Int -> [(Int, Int)]
pares n = [(i,j) | i <- [1..n], j <- [1..n], i < j, i /= j ]
