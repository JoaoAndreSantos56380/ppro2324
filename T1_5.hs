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
insert:: Ord a => a -> [a] -> [a]
insert elem [] = [elem]
insert elem (x:xs)
    | elem <= x = (elem:x:xs)
    | otherwise = (x:(insert elem xs))

{- (b) Defina uma função que implementa o algoritmo de ordenação por inserção,
   definido pelas duas regras: (i) a lista vazia está ordenada; (ii) uma lista
   não vazia pode ser ordenada ordenando a cauda e inserindo a cabeça no resultado. -}
insertSort:: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

{- Ficha 5, ex3. Determine um tipo e o valor para cada expressão. -}

{- (a) map (+1) [1..3] -}
ex3a = undefined

{- (b) map (>0) [3,-5,-2,0] -}
ex3b = undefined

{- (e) let f x = x * x in map (map f) [[1, 2], [3, 4, 5]] -}
ex3e = undefined

{- (f) filter (>5) [1..6] -}
ex3f = undefined

{- (h) filter (>0) (map (^2) [-3..3]) -}
ex3h = undefined

{- (i) map (^2) (filter (>0) [-3..3]) -}
ex3i = undefined


{- Ficha 5, ex4. Defina a função zipWith' :: (a->b->c) -> [a] -> [b] -> [c] semelhante à função zip
mas que aplica uma dada função a cada par de valores. -}

{- (a) Utilize uma função recursiva. -}
zipWith' :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{- (b) Tente agora uma solução com listas em compreensão. -}
zipWith'' :: (a-> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = [f x y | (x, y) <- zip xs ys]

{- (c) Defina a função zip utilizando a função zipWith. -}
zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = zipWith'' tuplo xs ys
    where tuplo x y = (x,y)


{- Ficha 5, ex8. Defina uma função total :: (Int -> Int) -> Int -> Int, de modo a que total f é
a função que, no ponto n, retorna f(0)+f(1)+...+f(n). -}

-- (a) Utilize uma função recursiva.
total:: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f x = f x + total f (x-1)

-- (b) Utilize uma lista em compreensão.
total':: (Int -> Int) -> Int -> Int
total' f x = sum [f y | y <- [1..x]]

-- (c) Utilize a função map
total'':: (Int -> Int) -> Int -> Int
total'' f x = sum (map f [1..x])

{- Ficha 5, ex9. Escreva a função mapif :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
que recebe um predicado e duas funções, e devolve uma lista em que cada elemento é
mapeado com a primeira ou segunda função, conforme o predicado desse elemento é
verdadeiro ou não.

ghci> mapif (>0) (*2) (\x -> 0) [1, -3, 7]
[2, 0, 14]
-}

-- (a) Utilize uma função recursiva.
mapif :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif f g h [] = []
mapif f g h (x:xs) = if f x then g x: mapif f g h xs else h x: mapif f g h xs

-- (b) Utilize uma lista em compreensão.
mapif' :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif' f g h xs = [if f x then g x else h x | x <- xs]

-- (c) Utilize a função map
mapif'' :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapif'' = undefined
