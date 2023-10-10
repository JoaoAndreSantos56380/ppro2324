{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o terceiro trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: tipos, classes de tipo, sintaxe das funções.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_03 where
import Data.Char

{-
Ficha 2, ex1. Que tipos usaria para representar:

a) Um ponto tridimensional
	-> (Int,Int,Int)
b) Um número de 1 a 10
	-> Int
c) Um polígono
	-> [(Double, Double)]
d) Um aluno: nome, número e nomes/notas das disciplinas feitas
	-> ([String], Int, [(String, Int)])
	nao confirmado para baixo da d)
e) Os alunos de uma turma
	-> lista de string
f) As palavras de um parágrafo
	-> String
g) Os parágrafos de um texto
	-> String
-}

{-
Ficha 2, ex2. Quais das seguintes frases são expressões Haskell?

a) ['a','b','c'] -> sim
b) ('a','b','c') -> sim
c) ['a',True] -> nao
d) [True, False] -> sim
e) ["a disciplina de PP", "é fixe"] -> sim
f) [('a',False),('b',True)] -> sim
g) [isDigit 'a', isLower 'f', isUpper 'h'] -> sim
h) (['a','b'],[False,True]) -> sim, lista de char e lista de bool | confirmado
i) [['a','b'],[False,True]] -> sim
j) [isDigit, isLower, isUpper] -> sim | nao sei
-}

{-
Ficha 2, ex6. Qual o tipo (definido sobre classes de tipo) das seguintes funções?

a) f1 x y = x < y -> Ord
b) f2 x y z = x == y || z -> Eq
c) f3 x y z = x == (y || z) -> Eq
d) f4 x y = show x ++ y -> show
e) f5 x y = show (x ++ y) -> show
f) f6 x y z = x + y > z -> Ord

Nota: para obter o tipo de uma qualquer expressão no ghci utilize o comando
:t <expressao>. Exemplos:
:t f1
:t show
:t show 1
-}

{-
Ficha 3, ex1. Usando pattern matching escreva funções que devolvam:
(a) O primeiro elemento de um par -}
primeiroPar :: (Int,Int) -> Int
primeiroPar (x,_) = x

{- (b) Um dado par com a ordem dos elementos trocados -}
trocaPar :: (Int, Int) -> (Int, Int)
trocaPar (x,y) = (y,x)

{- (c) O primeiro elemento de um triplo -}
primeiroTriplo :: (Int,Int,Int) -> Int
primeiroTriplo (x,y,z) = x

{- (d) Um dado triplo com os dois primeiros elementos trocados -}
trocaTriplo :: (Int,Int,Int) -> (Int,Int,Int)
trocaTriplo (x,y,z) = (y,x,z)

{- (e) O segundo elemento de uma lista -}
segundoLista :: [a] -> a
segundoLista xs
    | length xs < 2  = error "nao ha segundo elemento nesta lista"
    | length xs >= 2 = head (tail xs)

segundoListaPatternMatching :: [a] -> a
segundoListaPatternMatching = undefined {- nao faco ideia de como fazer -}



{- (f) O segundo elemento do primeiro par de uma lista de pares -}
segundoPrimeiroPar :: [(a,b)] -> b
segundoPrimeiroPar [] = error "lista vazia"
segundoPrimeiroPar (x:xs) = snd x

{- Conseguiria escrever as funções do exercício anterior sem
pattern matching? -}

primeiroPar'' = undefined -- será que dá?

trocaPar' = undefined -- será que dá?

primeiroTriplo' = undefined -- será que dá?

trocaTriplo' = undefined -- será que dá?

segundoLista' = undefined -- será que dá?

segundoPrimeiroPar' = undefined -- será que dá?

{-
Ficha 3, ex5. Qual a diferença entre as seguintes funções?
(a) f1 0 = 0
    f1 x = x-1
(b) f2 x = if x == 0 then 0 else x - 1
(c) f3 x = x-1
    f3 0 = 0
(d) f4 x | x /= 0    = x - 1
         | otherwise = 0

na alinea a) a funcao so restorna valores entre 0 e +infinito e usa pattern matching
na b) o mesmo que a a), a excecao de que usa um if
na c) o segundo padrao nunca e alcancado
ns d) sao usadas guardas
-}

{-
Ficha 3, ex6. Defina uma função que receba um par representando a
componente real e imaginária de um número complexo. Essa função
deverá devolver o quadrante em que esse ponto se encontra. -}
quadrante :: (Int, Int) -> Int
quadrante (x,y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    | otherwise = -1



{-
Ficha 3, ex9. Considere a função safetail que se comporta como tail,
excepto que transforma a lista vazia na lista vazia. Defina safetail
utilizando:
(a) uma expressão condicional -}
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

{- (b) equações guardadas -}
safetail' :: [a] -> [a]
safetail' xs
    | null xs    = []
    | otherwise  = tail xs

{- (c) pattern matching -}
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (x:xs) = xs




fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

guardaFibonacci :: Integer -> Integer
guardaFibonacci x
    | x == 0    = 1
    | x == 1    = 1
    | otherwise = guardaFibonacci (x-1) + guardaFibonacci (x-2)


fibeList :: Integer -> [Integer]
fibeList x = [fibonacci y | y <- [1..x]]


mid:: [(Int,Int,Int)] -> [Int]
mid xs = [ segundoTriplo tuple | tuple <- xs ]

segundoTriplo :: (Int,Int,Int) -> Int
segundoTriplo (x,y,z) = y

produtoInterno:: [(Int, Int)] -> Int
produtoInterno xs = sum [fst x * snd x | x <- xs ]


produtoInternoRecursivo:: [(Int, Int)] -> Int
produtoInternoRecursivo [] = 0
produtoInternoRecursivo ((x,y):xs) = (x * y) + produtoInternoRecursivo xs

halve:: [a] -> ([a],[a])
halve xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)
{- usar where para evitar repeticao -}
