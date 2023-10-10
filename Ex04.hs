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
halve :: [a] -> ([a],[a])
halve xs
    | length xs == 0 = ([],[])
    | length xs == 1 = (xs, [])
    | length xs == 2 = (take 1 xs, drop 1 xs)
    | otherwise = (take half xs , drop half xs)
    where half = length xs `div` 2

{- (b) Utilize a expressão let-in -}
halve' :: [a] -> ([a],[a])
halve' xs =
	let half = length xs `div` 2
	in (take half xs, drop half xs)

{-
Ficha 3, ex11. Escreva uma função que devolva o par de raízes de um polinómio do
segundo grau, assumindo que o polinómio tem raízes reais. Dado um polinómio da
forma ax^2 + bx + c, a função recebe três parâmetros a, b e c. Utilize o idioma where. -}
raizes = undefined

{-
Ficha 4, ex1. Defina as seguintes funções recursivamente:
(a) função que devolve a soma dos elementos de uma lista. -}
sum' = undefined

{- (b) função que devolve uma lista com n elementos idênticos.
   Se n for negativo, produz a lista vazia. -}
replicate' = undefined

{- (c) função que devolve o maior elemento de uma lista não vazia. -}
maximo = undefined

{- (d) função que decide se um dado elemento existe numa dada lista. -}
elem' = undefined

{- (e) função que substitui o primeiro elemento pelo segundo elemento na lista argumento. Por exemplo:
ghci> substitui 'a' 'o' "as balas"
"os bolos"                          -}
substitui  = undefined

{- (f) função que substitui todos os elementos da lista argumento que sejam menores que
   o segundo argumento, pelo terceiro argumento. -}
altera = undefined

{- (g) função que produz uma lista de pares a partir de duas listas. -}
zip' = undefined

{- (h) função que devolve uma lista com potências cuja base é o número dado no primeiro
   argumento e cujos expoentes são dados pelos valores do segundo argumento. -}
potencias = undefined

{- (i) função que devolve a string resultante de concatenar (mantendo a ordem)
   as strings dos pares contidos na lista dada como segundo argumento, que são
   iguais ao valor do primeiro argumento. -}
frase = undefined

{- (j) função que troca cada elemento de uma lista com o elemento seguinte,
   repetindo o processo de par em par de elementos. Se a lista contiver um
   número ímpar de elementos, o último elemento não é modificado. Por exemplo:
ghci trocaPares [1..5]
[2,1,4,3,5]                         -}
trocaPares = undefined

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
repBinaria = undefined

{- Ficha 4, ex3. Escreva uma função que decide se um dado número é um número odioso. Um número odioso é
um número não negativo que tem um número ímpar de uns na sua expansão binária. Os primeiros números
odiosos são 1, 2, 4, 7, 8, 11. -}
odioso = undefined
