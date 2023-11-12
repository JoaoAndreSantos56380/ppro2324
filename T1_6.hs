{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o sexto trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: funções de ordem superior.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_06 where

{- 11. Determine um tipo para cada uma das seguintes expressões lambda. -}

-- Em cada uma das alíneas deverá substituir " :: a" por uma assinatura correta

ex11a :: a
ex11a = \x -> x + 1

ex11b :: a
ex11b = (\x -> x + 1) 6

ex11c :: a
ex11c = \x -> x > 0

ex11d :: a
ex11d = \x y -> x + y

ex11e :: a
ex11e = (\x y -> x + y) 7

ex11f :: a
ex11f = (\x y -> x + y) 7 3

ex11g :: a
ex11g = \x -> (\y -> x + y)

ex11h :: a
ex11h = \f x -> f (f x)

ex11i :: a
ex11i = (\f x -> f (f x)) (\y -> y + 1)


{- 12. Escreva a função mult x y z = x * y * z utilizando uma expressão lambda. -}

mult = undefined


{- 13. Escreva as secções (++), (++[1,2]), ([1,2]++) como expressões lambda. Quais os seus tipos? -}

maisMais = undefined

maisMaisUmDois = undefined

umDoisMaisMais = undefined

{- 14. Utilizando uma expressão lambda, escreva uma função isNonBlank com a assinatura
Char -> Bool que devolve True apenas quando aplicada a caracteres não brancos, isto é,
para caracteres que não pertencem à lista [' ','\t','\n']. -}

isNonBlank :: Char -> Bool
isNonBlank = undefined

{- 18. Escreva as funções sum e length usando o foldl e foldr respectivamente. -}

sum' = undefined

length' = undefined

{- 19. Apresente definições para map e filter recorrendo à função foldr. -}

map' = undefined

filter' = undefined

{- 21. Escreva a função indexOf que recebe uma lista e um possível elemento dessa lista,
e devolve o primeiro índice onde esse elemento se encontra, ou -1 caso esse elemento
não se encontre na lista. -}

indexOf = undefined

{- 22. Um polinómio pode ser representado por uma lista de coeficientes.
Por exemplo, a lista [5,2,0,1,2] representa o polinómio
5x^4 + 2x^3 + 0x^2 + 1x^1 +2x^0 = 5x^4 + 2x^3 + x + 2.
Defina uma função poly :: Int -> [Int] -> Int que, dado um valor para x e um polinómio,
calcule o valor do polinómio nesse ponto. Utilize a função foldl ou foldr. -}

poly :: Int -> [Int] -> Int
poly = undefined
