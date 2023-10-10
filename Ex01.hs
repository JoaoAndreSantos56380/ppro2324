{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o primeiro trabalho para casa da disciplina de Princípios de Programação 2022/2023.

Endereça os seguintes tópicos: expressões, funções simples e listas.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido na aula da semana de 19 de setembro, não sendo necessário submeter para avaliação.
-}
module Ex_01 where

{-
O primeiro exercício já vem resolvido: escreva uma função que recebe dois números inteiros e devolva a sua soma.
-}

soma :: Int -> Int -> Int
soma x y = x + y

{-
Para exercitar a função soma carregue este ficheiro no interpretador de Haskell, escrevendo numa linha de comandos

ghci Ex01.hs

Faça depois uns pequenos testes, escrevendo no prompt

soma 2 2
soma 0 1
soma 3 (-4)

e observe o resultado.

Os exercícios abaixo são para serem desenvolvidos em casa, antes da aula.

1a _ Escreva uma função que receba três inteiros e que devolve a sua soma.
-}

soma3 :: Int -> Int -> Int -> Int
soma3 x y z = x + y + z

{-
Neste caso temos de trocar a expressão undefined por uma expressão adequada. Depois de ter feito tal, volte a carregar o ficheiro no interpretador Haskell, batendo

:r

no prompt do interpretador.
-}

{-
1b _ Escreva uma função que receba três inteiros e que devolve a sua soma se forem todos positivos e zero caso contrário.
-}

soma3Pos :: Int -> Int -> Int -> Int
soma3Pos x y z = if x > 0 && y > 0 && z > 0 then x+y+z else 0

{-
2 _ Escreva uma função que receba três inteiros e devolva True se a distância entre os dois primeiros for inferior ao terceiro e False caso contrário. Assuma que o terceiro inteiro é não negativo. Sugestão: utilize a função abs para obter o valor absoluto de um número.
-}

distancia:: Int->Int->Int->Bool
distancia x y z = if abs(x-y) < z then True else False
{- distancia x y z = abs(x-y) < z -}

{-
3 _ Escreva uma função juntaDigito que receba dois inteiros, o segundo dos quais entre 0 e 9, e que devolva o inteiro resultante de acrescentar o segundo no fim do primeiro. Por exemplo:
-}

juntaDigito :: Int -> Int -> Int
juntaDigito n d = if n > 0 then n * 10 + d else n * 10 - d

{-
Por exemplo

juntaDigito (-123) 4
-1234

4 _ Responda a estas questões numa folha de papel antes de testar as suas respostas.
Quantos elementos tem cada uma das seguintes listas?

a) ['a','b'] 						=> 2 nice
b) "ab" 							=> 2 nice
c) [['a','b']]						=> 1 nice
d) [['a','b'], ['c','d']]			=> 2 nice
e) [[['a','b']]]					=> 1 nice
f) []								=> 0 nice
g) [[]]								=> 1 nice
h) [[],[]]							=> 2 nice

Para verificar a resposta utilize a função length no interpretador de Haskell. Por exemplo

length [['a','b'], ['c','d']]
-}

{-
5 _ Para resolver os exercícios abaixo considere as seguintes funções constantes no Prelude.

• 1 : [2,3] _ devolve a lista obtida pela junção de um elemento à cabeça de uma lista, [1,2,3].
• head [1,2,3] _ devolve o primeiro elemento de uma lista, 1.
• tail [1,2,3] _ devolve a lista excluindo o primeiro elemento,
[2,3].
• last [1,2,3] _ devolve o último elemento de uma lista, 3.
• init [1,2,3] _ devolve a lista excluindo o último elemento, [1,2].
• null [1,2,3] _ devolve true se a lista for vazia, False.
• length [1,2,3] _ devolve o número de elementos na lista, 3.
• reverse [1,2,3] _ devolve a lista, mas em ordem inversa, [3,2,1].
• take 2 [1,2,3] _ devolve os primeiros 2 (ou n) elementos da lista, [1,2].
• sum [1,2,3] _ devolve a soma dos elementos da lista, 6.

Antes de tentar escrever as funções sugeridas abaixo, exercite as funções do Prelude. Por exemplo:

head [1,2,3]
head [1]
head []
head 1

Depois escreva as seguintes funções.

5(a) Uma função que devolva True se uma dada lista tem mais que 10 elementos, e que devolva False caso contrário.
-}

listaGrande :: [a] -> Bool
listaGrande xs = length xs > 10

{-
5(b) Uma função que verifica se uma lista não é vazia.
-}

naoVazia :: [a] -> Bool
naoVazia xs = length xs > 0
{- naoVazia xs = not (null xs) -}

{-
5(c) Uma função que retira o primeiro e o último caracter de uma String.
-}

retiraExtremos :: [a] -> [a]
retiraExtremos xs = tail (init xs)

{-
5(d) Uma função que devolve o segundo elemento de uma lista. Chame-a de segundo.
-}

segundo :: [a] -> a
segundo xs = head (tail xs)

{-
5(e) Uma função que devolve o penúltimo elemento de uma lista. Chame-a de penultimo.
-}

penultimo :: [a] -> a
penultimo xs = last (init xs)

{-
5(f) Uma função que devolve o n-ésimo elemento de uma lista. Assuma que os índices começam em zero e que n está entre 0 e o comprimento da lista menos um. Reescreva as duas funções anteriores. Chame-a nesimo.
-}

nesimo :: [a] -> Int -> a
nesimo xs x = last (take x xs)

segundo' :: [Int] -> Int
segundo' xs = nesimo xs 1

penultimo':: [Int] -> Int
penultimo' xs = nesimo xs (length xs - 2)

{-
5(g) Uma função que inverte todos os elementos de uma lista excepto o primeiro. O primeiro elemento da lista permanece na primeira posição. Chame-a inverteCauda.
-}

inverteCauda :: [Int] -> [Int]
inverteCauda xs = head xs : reverse(tail xs)

{-
5(h) Uma função que calcula o somatório dos primeiros 5 elementos de uma lista. Chame-a soma5.
-}

soma5 :: [Int] -> Int
soma5 xs = sum (take 5 xs)

{-
5(i) Uma função que calcula o somatório dos primeiros n elementos de uma lista. Chame-a somaPrimeiros.
-}

somaPrimeiros :: [Int] -> Int -> Int
somaPrimeiros xs n = sum (take n xs)

{-
5(h') Reescreva a função da alínea anterior utilizando este resultado.
-}

soma5' :: [Int] -> Int
soma5' xs = somaPrimeiros xs 5

{-
5(j) Uma função que recebe duas listas e devolve verdadeiro se a) o último elemento de ambas as listas for igual, b) as listas tiverem o mesmo comprimento, e c) as listas forem não nulas.
-}

fallen :: [Int] ->[Int] -> Bool
fallen xs ys = lastequal xs ys &&  lengthequal xs ys && notnull xs ys

lastequal:: [Int] -> [Int] -> Bool
lastequal xs ys = last xs == last ys

lengthequal:: [Int] -> [Int] -> Bool
lengthequal xs ys = length xs == length ys

notnull:: [Int] -> [Int] -> Bool
notnull xs ys = length xs /= 0 && length ys /= 0
