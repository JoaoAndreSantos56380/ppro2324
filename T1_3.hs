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
b) Um número de 1 a 10
c) Um polígono
d) Um aluno: nome, número e nomes/notas das disciplinas feitas
e) Os alunos de uma turma
f) As palavras de um parágrafo
g) Os parágrafos de um texto

-}

{-
Ficha 2, ex2. Quais das seguintes frases são expressões Haskell?

a) ['a','b','c']
b) ('a','b','c')
c) ['a',True]
d) [True, False]
e) ["a disciplina de PP", "é fixe"]
f) [('a',False),('b',True)]
g) [isDigit 'a', isLower 'f', isUpper 'h']
h) (['a','b'],[False,True])
i) [['a','b'],[False,True]]
j) [isDigit, isLower, isUpper]
-}

{-
Ficha 2, ex6. Qual o tipo (definido sobre classes de tipo) das seguintes funções?

a) f1 x y = x < y
b) f2 x y z = x == y || z
c) f3 x y z = x == (y || z)
d) f4 x y = show x ++ y
e) f5 x y = show (x ++ y)
f) f6 x y z = x + y > z

Nota: para obter o tipo de uma qualquer expressão no ghci utilize o comando
:t <expressao>. Exemplos:
:t f1
:t show
:t show 1
-}

{-
Ficha 3, ex1. Usando pattern matching escreva funções que devolvam:
(a) O primeiro elemento de um par -}
primeiroPar:: (a,b) -> a
primeiroPar (x,y) = x

{- (b) Um dado par com a ordem dos elementos trocados -}
trocaPar :: (a,b) -> (b,a)
trocaPar (x,y) = (y,x)

{- (c) O primeiro elemento de um triplo -}
primeiroTriplo:: (a,b,c) -> a
primeiroTriplo (x,y,z) = x

{- (d) Um dado triplo com os dois primeiros elementos trocados -}
trocaTriplo:: (a,b,c) -> (b,a,c)
trocaTriplo (x,y,z) = (y,x,z)

{- (e) O segundo elemento de uma lista -}
segundoLista:: [a] -> a
segundoLista xs
    | length xs < 2 = error "nao ha segundo aqui"
    | length xs >= 2 = head(tail xs)


{- (f) O segundo elemento do primeiro par de uma lista de pares -}
segundoPrimeiroPar:: [(a,b)] -> b
segundoPrimeiroPar xs = snd (head xs)

{- Conseguiria escrever as funções do exercício anterior sem
pattern matching? -}
primeiroPar'':: (a,b) -> a
primeiroPar'' par = fst par -- será que dá?

trocaPar':: (a,b) -> (b,a)
trocaPar' par = (snd par, fst par) -- será que dá?

primeiroTriplo' = undefined -- será que dá?

trocaTriplo' = undefined -- será que dá?

segundoLista':: [a] -> a
segundoLista' [] = error "errore" -- será que dá?
segundoLista' [x] = error "errore2" -- será que dá?
segundoLista' (x:[y]) = head [y] -- será que dá?
segundoLista' (x:y:[z]) = y -- será que dá?

segundoPrimeiroPar':: [(a,b)] -> b
segundoPrimeiroPar' xs = snd (segundoLista' xs) -- será que dá?

{-
Ficha 3, ex5. Qual a diferença entre as seguintes funções?
(a) f1 0 = 0
    f1 x = x-1
(b) f2 x = if x == 0 then 0 else x - 1
(c) f3 x = x-1
    f3 0 = 0
(d) f4 x | x /= 0    = x - 1
         | otherwise = 0
-}

{-
Ficha 3, ex6. Defina uma função que receba um par representando a
componente real e imaginária de um número complexo. Essa função
deverá devolver o quadrante em que esse ponto se encontra. -}
quadrante:: (Int,Int) -> Int
quadrante (x,y)
    | x > 0 && y > 0 = 1
    | x < 0 && y > 0 = 2
    | x < 0 && y < 0 = 3
    | x > 0 && y < 0 = 4
    |otherwise = -1


{-
Ficha 3, ex9. Considere a função safetail que se comporta como tail,
excepto que transforma a lista vazia na lista vazia. Defina safetail
utilizando:
(a) uma expressão condicional -}
safetail:: [a] -> [a]
safetail xs = if length xs > 1 then tail xs else xs

{- (b) equações guardadas -}
safetail':: [a] -> [a]
safetail' xs
    | length xs > 1 = tail xs
    | otherwise = xs

{- (c) pattern matching -}
safetail'':: [a] -> [a]
safetail'' [] = []
safetail'' [x] = [x]
safetail'' (x:xs) = xs
