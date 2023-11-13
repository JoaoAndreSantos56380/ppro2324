{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
primeiroPar :: (a,b) -> a
primeiroPar (x, _) = x

parTrocado :: (a,b) -> (b, a)
parTrocado (x, y) = (y, x)

primeiroTriplo :: (a,b,c) -> c
primeiroTriplo (_, _, z) = z

triploTrocado :: (a,b,c) -> (b, a, c)
triploTrocado (x, y, z) = (y, x, z)

quadrante :: (Int, Int) -> Int
quadrante (x, y)
            | x < 0 && y < 0 = 3
            | x > 0 && y < 0 = 4
            | x < 0 && y > 0 = 2
            | x > 0 && y > 0 = 1
            | otherwise = 0




--SEMANA 3
--Ficha 3
--10
halve :: [a] -> ([a], [a])
halve xs = (ys, zs)
    where n = length xs
          m = if even n then n `div` 2 else (n - 1) `div` 2
          ys = take m xs
          zs = drop m xs

--11
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (x, y)
    where
        x = d + sqrt e / (2*a)
        y = d - sqrt e / (2*a)
        d = -b / (2 * a)
        e = b * b - (4 * a * c)

--SEMANA 4
--Ficha 4
--1.a
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--e
substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui a b (x:xs)
    | a == x = b:substitui a b xs
    | otherwise = x:substitui a b xs

--j
trocaPares :: [a] -> [a]
trocaPares [] = []
trocaPares [x] = [x]
trocaPares (x:y:xs) = y:x:trocaPares xs

--m

posicoes :: [Int] -> Int -> [Int]
posicoes xs n = indexP xs n 0

indexP :: [Int] -> Int -> Int -> [Int]
indexP [] _ _ = []
indexP xs 0 _ = xs
indexP (x:xs) n m
    | x `mod` n == 0 = m:indexP xs n (m+1)
    | otherwise = indexP xs n (m+1)


--2    
bin :: Int -> Int
bin x
    | x < 2 = x
    | otherwise = bin ( x `div` 2) * 10 + x `mod` 2

--3
odioso :: Int -> Bool
odioso x = odd (odiosoAux (bin x))

odiosoAux :: Int -> Int
odiosoAux 0 = 0
odiosoAux x = x `mod` 10 + odiosoAux (x `div` 10)


--SEMANA 5
--FICHA 5
--4
--a)
zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipWith'' :: (a->b->c)->[a]->[b]->[c]
zipWith'' f xs ys = [ f x y | (x, y) <- zip xs ys]

zip' :: [a]->[b]->[(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' xs ys = zipWith' (,) xs ys

--7
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs)
    | f x = x:dropUntil f xs
    | otherwise = []


--8
--c)
total :: (Int -> Int) -> Int -> Int
total f x =  sum (map f [1..x])

--9
--a)
aplica :: [a -> a] -> [a] -> [a]
aplica [] xs = xs
aplica (f:fs) xs = aplica fs (map f xs)

--13
isNonBlank :: Char -> Bool
isNonBlank x = not (x `elem` [' ','\t','\n'])

--17
--length' :: [a] -> Int
--length' = foldr (\x y -> 1 + y) 0

--pratica
rev :: [a] -> [a]
rev = foldl (flip(:)) [] 

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map ((:) x) acc) []