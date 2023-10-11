{- module P1_fc56380 where -}

baralho :: [String]
baralho = [valor ++ naipe | valor <- ["2", "3", "4", "5", "6", "7", "8", "9", "A", "T", "J", "Q", "K"], naipe <- ["S", "H", "D", "C"]]

half :: [a] -> ([a],[a])
half xs
    | length xs == 0 = ([],[])
    | length xs == 1 = (xs, [])
    | length xs == 2 = (take 1 xs, drop 1 xs)
    | otherwise = (take halfe xs , drop halfe xs)
    where halfe = length xs `div` 2

cardValue:: String -> [Int]
cardValue xs
    | halfe == "A" = [1, 11]
    | halfe == "2" = [2]
    | halfe == "3" = [3]
    | halfe == "4" = [4]
    | halfe == "5" = [5]
    | halfe == "6" = [6]
    | halfe == "7" = [7]
    | halfe == "8" = [8]
    | halfe == "9" = [9]
    | halfe == "T" = [10]
    | halfe == "J" = [10]
    | halfe == "Q" = [10]
    | halfe == "K" = [10]
    where halfe = fst (half xs)


combinacoesBlackjack :: Int -> [(String, String)]
combinacoesBlackjack points = removeDuplicatesTuple [ (card1, card2) | card1 <- baralho, card2 <- baralho, value1 <- (cardValue card1), value2 <- (cardValue card2), (value1 + value2) == points, card1 /= card2 ]

removeDuplicatesTuple :: (Ord a) => [(a, a)] -> [(a, a)]
removeDuplicatesTuple xs = [(a, b) | i <- [0..length sortedPairs - 1],
                               let (a, b) = sortedPairs !! i,
                               not (elem (a, b) (take i sortedPairs))]
    where sortedPairs = [(min a b, max a b) | (a, b) <- xs]

fullHouses = removeFirstThreeDuplicates ([ (card1, card2, card3, card4, card5) | card1 <- baralho, card2 <- baralho, card3 <- baralho, card4 <- baralho, card5 <- baralho, head card1 == head card2 && head card1 == head card3 && head card1 /= head card4 && head card4 == head card5 && tail card1 /= tail card2 && tail card1 /= tail card3 && tail card2 /= tail card3 && tail card4 /= tail card5 ])


-- A função removeFirstThreeDuplicates está a funcionar bem. Só falta agrupar o (take i sortedTriple) pela quarta (ou quinta) carta, e aplicar o removeFirstThreeDuplicates a cada um desses grupos. Neste momento, estamos a aplicar o removeFirstThreeDuplicates aos grupos todos, resultando nisto: [("2D","2H","2S","3S","3H"),("2C","2H","2S","3S","3H"),("2C","2D","2S","3S","3H"),("2C","2D","2H","3S","3H"), ...] mas depois não temos os grupos de 4, 5, 6, 7, 8, 9, K, D, J e Q.
removeFirstThreeDuplicates xs = [(a, b, c, d, e) | i <- [0..length sortedTriple - 1],
                               let (a, b, c, d, e) = sortedTriple !! i,
                               not ([a, b, c] `elem` (removeFirstThreeDuplicatesAux (take i sortedTriple)))]
    where sortedTriple = [(minimum [a, b, c], minimum [x | x <- [a, b, c], x /= minimum [a, b, c]], maximum [a, b, c], d, e) | (a, b, c, d, e) <- xs]

removeFirstThreeDuplicatesAux xs = [[a, b, c] | (a, b, c, d, e) <- xs]

removeFirstThreeDuplicatesAux2 x xs =

removeLastTwoDuplicates xs = [(a, b, c, d, e) | i <- [0..length sortedTuple - 1],
                               let (a, b, c, d, e) = sortedTuple !! i,
                               not ([d, e] `elem` (removeLastTwoDuplicatesAux (take i sortedTuple)))]
    where sortedTuple = [(a, b, c, min d e, max d e) | (a, b, c, d, e) <- xs]

removeLastTwoDuplicatesAux xs = [[d, e] | (a, b, c, d, e) <- xs]

{- removeDuplicatesTuples xs = [(a, b, c, d, e) | i <- [0..length sortedPairs - 1],
                               let (a, b, c, d, e) = sortedPairs !! i,
                               not (elem (a, b, c, d, e) (take i sortedPairs))]
    where sortedPairs = [(minimum [a, b, c, d, e], minimum [x | x <- [a, b, c, d, e], x /= minimum [a, b, c, d, e]], minimum [x | x <- [a, b, c, d, e], x /= minimum [a, b, c, d, e] && x /= minimum [x | x <- [a, b, c, d, e], x /= minimum [a, b, c, d, e]]], maximum [x | x <- [a, b, c, d, e], x /= maximum [a, b, c, d, e]], maximum [a, b, c, d, e]) | (a, b, c, d, e) <- xs] -}

{- sort l = [y | x <- [minBound..], y <- l, y == x] -}

main = do
  let full = fullHouses
  print full
  print (length full)

{- removeDuplicatesString :: (Ord a) => [[a]] -> [[a]]
removeDuplicatesString xs = [a | i <- [0..length sortedPairs - 1],
                               let a = sortedPairs !! i,
                               not (elem a (take i sortedPairs))]
    where sortedPairs = [a | a <- xs] -}






{- valueHandAux [card1, card2] = [ card1Value+card2Value | [card1Value, card2Value] <- [ [card1Values, card2Values] | card1Values <- cardValue(card1), card2Values <- cardValue(card2)]]

valueHand [card1, card2] = [value | value <- valueHandAux [card1, card2], not (value `elem` valueHandAux [card1, card2])] -}

--combinacoesBlackjackAux :: Int -> [(String, String)]
--combinacoesBlackjackAux points = [ (card1, card2) | card1 <- baralho, card2 <- baralho, (cardValue card1) + (cardValue card2) == [points], card1 /= card2]



{-
combinacoesBlackjack :: Int -> [(String, String)]
combinacoesBlackjack points = [tuplo | tuplo <- (combinacoesBlackjackAux points)] -- , not (tuplo `elem` combinacoesBlackjackAux)

evitaReps :: [(String, String)] -> [(String, String)]
evitaReps xs = undefined

{- combinacoesBlackjack' :: Int -> [(String, String)]
combinacoesBlackjack' x = [(card1, card2) | (card1:rest) <- tails baralho, card2 <- rest, valorCarta card1 + valorCarta card2 == x] -}



combinacoes::[String] -> [(String, String)]
combinacoes xs = [ (x,y) | x <- xs, y <- xs, x /= y]

{- gera as cartas possiveis para um valor -}
cartas:: String -> [String]
cartas xs = [xs ++ naipe | naipe <- ["S", "H", "D", "C"]]

{- gera trios a partir das 4 cartas dadas -}
geratrios:: [String] -> [[String]]
geratrios cartas = undefined
{- geratrios xs = [ [x,y,z] | x <- xs, y <- xs, z <- xs, x /= y, x /= z, y/= z] -}

{-
1 2 3 4

1 234 34

123 124 134

2 34

234


-}


{- gera pares a partir das 4 cartas dadas -}
gerapares:: [String] -> [[String]]
gerapares cartas = [ [x,y] | (x:xs) <- [xs | xs <- tails cartas], y <- xs]
{-
1 2 3 4
1 234
12 13 14

2 34
23 24

3 4
34
-}

{- gera combinacoes de fullhouse a partir de um par e um trio -}
gerafulls:: [[String]] -> [[String]] -> [[String]]
gerafulls trio par = [ x ++ y| x <- trio, y <- par ]

fullHouses:: [[String]]
fullHouses = undefined{- [[trio, par]| trio <- trios, par <- pares] -}

-}
