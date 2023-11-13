{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' (*) (x:xs) (y:ys) = (*) x y : zipWith' (*) xs ys 


listaZip :: (Num a, Enum a) => [a] -> [a]
listaZip ys = zipWith (*) ys [0,1..]

sum' :: Num c => [c] -> c
sum' [x] = x
sum' [] = 0
sum' (x:xs) = x + sum' xs

soma :: [Int] -> Int
soma = sum' (listaZip ys)

