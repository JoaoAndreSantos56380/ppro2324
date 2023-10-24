{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o quinto trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: recursão, funções de ordem superior.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_05 where

{- Ficha 4, ex5. Programe o seguinte algoritmo de ordenação por inserção em dois passos.
   (a) Defina uma função que insere um elemento na posição correcta dentro de
   uma lista ordenada. -}
insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
    | e <= x = e : x : xs
    | otherwise = x : insert e xs

{- (b) Defina uma função que implementa o algoritmo de ordenação por inserção,
   definido pelas duas regras: (i) a lista vazia está ordenada; (ii) uma lista
   não vazia pode ser ordenada ordenando a cauda e inserindo a cabeça no resultado. -}

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

{- Ficha 5, ex3. Determine um tipo e o valor para cada expressão. -}

{- (a) map (+1) [1..3] -}
ex3a = [2,3,4]

{- (b) map (>0) [3,-5,-2,0] -}
ex3b = [True, False, False, False]

{- (e) let f x = x * x in map (map f) [[1, 2], [3, 4, 5]] -}
ex3e = [[1,4],[9,16,25]]

{- (f) filter (>5) [1..6] -}
ex3f = [6]

{- (h) filter (>0) (map (^2) [-3..3]) -}
ex3h = [9,4,1,1,4,9]

{- (i) map (^2) (filter (>0) [-3..3]) -}
ex3i = [1,4,9]


{- Ficha 5, ex4. Defina a função zipWith' :: (a->b->c) -> [a] -> [b] -> [c] semelhante à função zip
mas que aplica uma dada função a cada par de valores. -}

{- (a) Utilize uma função recursiva. -}
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


{- (b) Tente agora uma solução com listas em compreensão. -}
zipWith'' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = [f x y | (x,y) <- zip xs ys ]

{- (c) Defina a função zip utilizando a função zipWith. -}
zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys =
    let tuplo x y = (x,y)
    in zipWith' tuplo xs ys


{- Ficha 5, ex8. Defina uma função total :: (Int -> Int) -> Int -> Int, de modo a que total f é
a função que, no ponto n, retorna f(0)+f(1)+...+f(n). -}

-- (a) Utilize uma função recursiva.
total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f x = f x + total f (x - 1)

-- (b) Utilize uma lista em compreensão.
total' :: (Int -> Int) -> Int -> Int
total' f n  = sum [f x | x <- [0..n]]

-- (c) Utilize a função map
total'' :: (Int -> Int) -> Int -> Int
total'' f n = sum (map f [0..n])

{- Ficha 5, ex9. Escreva a função mapif :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
que recebe um predicado e duas funções, e devolve uma lista em que cada elemento é
mapeado com a primeira ou segunda função, conforme o predicado desse elemento é
verdadeiro ou não.

ghci> mapif (>0) (*2) (\x -> 0) [1, -3, 7]
[2, 0, 14]
-}

-- (a) Utilize uma função recursiva.
mapif :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif _ _ _ [] = []
mapif p f1 f2 (x:xs)
    | p x = f1 x : mapif p f1 f2 xs
    | otherwise = f2 x : mapif p f1 f2 xs

-- (b) Utilize uma lista em compreensão.
mapif' :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif' p f1 f2 xs = [if p x then f1 x else f2 x | x <- xs]

-- (c) Utilize a função map
mapif'' :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif'' p f1 f2 xs = map h xs
    where h x = if p x then f1 x else f2 x
