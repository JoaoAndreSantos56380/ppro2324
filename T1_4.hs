{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o quarto trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: sintaxe das funções, recursão.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_04 where

{-
Ficha 3, ex10. Utilizando as funções sobre listas constantes no Prelude,
escreva a função halve :: [a] -> ([a],[a]) que separa uma lista em duas
sublistas com comprimentos que não difiram de mais de uma unidade. Por exemplo:
ghci> halve [1..6]
([1,2,3],[4,5,6])

(a) Utilize o idioma where -}
halve:: [a] -> ([a],[a])
halve xs = (esquerda, direita)
  where esquerda = take (length xs `div` 2) xs
        direita  = drop (length xs `div` 2) xs
{- (b) Utilize a expressão let-in -}
halve':: [a] -> ([a], [a])
halve' xs =
    let esquerda = take (length xs `div` 2) xs
        direita  = drop (length xs `div` 2) xs
    in  (esquerda,direita)

{-
Ficha 3, ex11. Escreva uma função que devolva o par de raízes de um polinómio do
segundo grau, assumindo que o polinómio tem raízes reais. Dado um polinómio da
forma ax^2 + bx + c, a função recebe três parâmetros a, b e c. Utilize o idioma where. -}
raizes:: Float -> Float -> Float -> (Float, Float)
raizes x y z = ( (-y + (sqrt delta)) / (2 * x), (-y - (sqrt delta)) / (2 * x) )
    where delta = y^2 - 4 * x * z

{-
Ficha 4, ex1. Defina as seguintes funções recursivamente:
(a) função que devolve a soma dos elementos de uma lista. -}
sum':: [a] -> Int
sum' [] = 0
sum' [a] = 1
sum' (x:xs) = 1 + sum' xs

{- (b) função que devolve uma lista com n elementos idênticos.
   Se n for negativo, produz a lista vazia. -}
replicate':: a -> Int -> [a]
replicate' x n
    | n <= 0    = []
    | otherwise = x : replicate' x (n-1)

{- (c) função que devolve o maior elemento de uma lista não vazia. -}
maximo:: [Int] -> Int
maximo xs = maxAux xs (minBound::Int)

maxAux :: [Int]-> Int-> Int
maxAux (x:xs) max
    | xs == [] = if x > max then x else max
    | x > max = maxAux xs x
    | otherwise = maxAux xs max

{- (d) função que decide se um dado elemento existe numa dada lista. -}
elem' :: (Eq a) => [a] -> a -> Bool
elem' xs a
    | xs == [] = False
    | otherwise = elemAux' xs a

elemAux' :: (Eq a) => [a] -> a -> Bool
elemAux' (x:xs) a
    | xs == [] = x == a
    | otherwise = if x == a then True else elem' xs a

{- (e) função que substitui o primeiro elemento pelo segundo elemento na lista argumento. Por exemplo:
ghci> substitui 'a' 'o' "as balas"
"os bolos"                          -}

substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui x y (z:xs)    | x == z = y : substitui x y xs
                        | otherwise = z : substitui x y xs

{- (f) função que substitui todos os elementos da lista argumento que sejam menores que
   o segundo argumento, pelo terceiro argumento. -}
altera = undefined

{- (g) função que produz uma lista de pares a partir de duas listas. -}
zip' = undefined

{- (h) função que devolve uma lista com potências cuja base é o número dado no primeiro
   argumento e cujos expoentes são dados pelos valores do segundo argumento. -}
potencias:: Int -> [Int] ->[Int]
potencias base (expoente:expoentes) = if expoentes == [] then [(base^expoente)] else (base ^ expoente) : potencias base expoentes

{- (i) função que devolve a string resultante de concatenar (mantendo a ordem)
   as strings dos pares contidos na lista dada como segundo argumento, que são
   iguais ao valor do primeiro argumento. -}
frase = undefined

{- (j) função que troca cada elemento de uma lista com o elemento seguinte,
   repetindo o processo de par em par de elementos. Se a lista contiver um
   número ímpar de elementos, o último elemento não é modificado. Por exemplo:
ghci trocaPares [1..5]
[2,1,4,3,5]                         -}
trocaPares:: [Int] -> [Int]
trocaPares [] = []
trocaPares [x] = [x]
trocaPares (x:y:xs) = y:x:(trocaPares xs)

{- (k) função que dadas duas listas associação, devolve uma outra lista associação,
   obtida por junção das duas listas. Neste exercício estamos apenas interessados em
   listas ordenadas, por ordem crescente dos valores das chave. Se cada uma das
   listas contiver um par com a mesma chave, o valor associado a essa chave será
   a soma dos valores correspondentes nas duas listas. -}
fusao = undefined

{- (l) função que devolve uma lista contendo os elementos de uma dada lista
   que são múltiplos de um também dado número inteiro. -}
multiplos = undefined

{- (m) função que devolve uma lista contendo as posições dos elementos da lista dada como primeiro argumento
que são múltiplos do segundo argumento. Por exemplo:
ghci> posicoes [1,3,6,2,5,15,3,5,7,18] 3
[1,2,5,6,9]                             -}
posicoes = undefined

{-
Ficha 4, ex2. Um número inteiro positivo n pode ser convertido para representação binária através do
seguinte algoritmo:
  i) se n < 2, a sua representação binária é o próprio n;
 ii) caso contrário, divide-se n por 2. O resto (0 ou 1) dá-nos o último dígito (o mais à direita) da
     representação binária;
iii) os dígitos precedentes da representação binária são dados pela representação binária do quociente de
     n por 2.
Escreva uma função que dado num inteiro devolve a sua representação binária. Por exemplo:
ghci> repBinaria 23
10111                    -}
repBinaria:: Int -> Int
repBinaria n
    | n < 2 = n
    | otherwise = 10 * repBinaria (n `div` 2) + n `mod` 2

{- Ficha 4, ex3. Escreva uma função que decide se um dado número é um número odioso. Um número odioso é
um número não negativo que tem um número ímpar de uns na sua expansão binária. Os primeiros números
odiosos são 1, 2, 4, 7, 8, 11. -}

{- numeroUns:: Int -> Int
numeroUns x
    | x < 2 = x
    | otherwise = (x `mod` 2) + numeroUns (x `div` 2) -}
numeroUns :: Int -> Int
numeroUns x
    | x <= 0    = 0
    | x == 1    = 1
    | otherwise = (x `mod` 2) + numeroUns (x `div` 2)


odioso:: Int -> Bool
odioso x = odd(numeroUns x)

-- 23 = 10 * rep11(10* rep5(10 * rep2(10 * rep1 + 0) + 1) + 1) + 1
