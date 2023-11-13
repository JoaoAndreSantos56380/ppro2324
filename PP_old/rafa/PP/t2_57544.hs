posicaoInicial :: [String] -> (Int, Int)
posicaoInicial [] = (0, 0)
posicaoInicial [[]] = (0, 0)
posicaoInicial [_:_] = (0,0)
posicaoInicial (_:y:ys)
    | 'S' `elem` y = ((length y - 1) - length ys, posicaoAux y 'S' 1)
    |otherwise = posicaoInicial (y:ys)

posicaoFinal :: [String] -> (Int, Int)
posicaoFinal [] = (0, 0)
posicaoFinal [[]] = (0, 0)
posicaoFinal [_:_] = (0, 0)
posicaoFinal (_:y:ys)
    | 'F' `elem` y = ((length y - 1) - length ys, posicaoAux y 'F' 1)
    |otherwise = posicaoFinal (y:ys)

posicaoAux :: String -> Char -> Int -> Int
posicaoAux [] _ _ = 0
posicaoAux [_] _ _ = 0
posicaoAux (_:y:ys) a z
    | y == a = z
    | otherwise = posicaoAux (y:ys) a z+1

vizinhos :: [String] -> (Int, Int) -> [(Int, Int)]
vizinhos xs (x, y) = vizinhosAux xs (possiveisVizinhos (x, y))

vizinhosAux :: [String] -> [(Int, Int)] -> [(Int, Int)]
vizinhosAux _ [] = []
vizinhosAux xs (y:ys) = filter (\y -> descobrePos y xs /= '*') (y:ys)

descobrePos :: (Int, Int) -> [String] -> Char
descobrePos (x, y) zs = (zs !! x) !! y

possiveisVizinhos :: (Int, Int) -> [(Int, Int)]
possiveisVizinhos (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]


procuraCaminho :: [String] -> Bool
procuraCaminho xs = posicaoFinal xs `elem` removeDuplicados (procuraCaminhoAux xs [posicaoInicial xs] (vizinhos xs (posicaoInicial xs)))

procuraCaminhoAux :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
procuraCaminhoAux [] _ _ = []
procuraCaminhoAux _ _ [] = []
procuraCaminhoAux _ [] _ = []
procuraCaminhoAux xs ys zs 
    | posicaoFinal xs `elem` zs = ys ++ [posicaoFinal xs]
    | otherwise = procuraCaminhoAux xs (ys ++ zs) (filtraRepetidos xs ys (porVizitar xs zs))
    

porVizitar :: [String] -> [(Int, Int)] -> [(Int, Int)]
porVizitar xs = foldr ((++) . vizinhos xs) []

filtraRepetidos :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
filtraRepetidos _ _ [] = []
filtraRepetidos _ [] zs = zs
filtraRepetidos xs (y:ys) zs
    | y `elem` zs = filtraRepetidos xs ys (filter (/=y) zs)
    | otherwise = filtraRepetidos xs ys zs

removeDuplicados :: [(Int, Int)] -> [(Int, Int)]
removeDuplicados [] = []
removeDuplicados (x:xs)
    | x `elem` xs = removeDuplicados xs
    | otherwise = x:removeDuplicados xs

    

lab1 :: [String]
lab1 = ["*****","*S*F*","* * *","*   *","*****"]
lab2 :: [String]
lab2 = ["*****","*   *","*   *","* SF*","*****"]
lab3 :: [String]
lab3 = ["*****","*S***","*****","***F*","*****"]
lab4 :: [String]
lab4 = ["******","*S   *","**** *","***  *","*F   *","******"]
lab5 :: [String]
lab5 = ["*****","*  F*","*   *","*S  *","*****"]

colocaP :: [String] -> [String]
colocaP [] = []
colocaP (z:zs) 
        | 'S' `elem` z = substitui z : colocaP zs 
        | otherwise = z : colocaP zs

substitui :: String -> String
substitui [] = []
substitui (z:zs)
    | z == 'S' = 'P' : substitui zs
    | otherwise = z : substitui zs

