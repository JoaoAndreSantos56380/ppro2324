{- module P1_fc56380 where -}

baralho :: [String]
baralho = [valor ++ naipe | valor <- ["A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K"], naipe <- ["S", "H", "D", "C"]]

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
combinacoesBlackjack points = removeDuplicatesTuple [ (card1, card2) | card1 <- baralho, card2 <- baralho, value1 <- (cardValue card1), value2 <- (cardValue card2), (value1 + value2) == points ]

removeDuplicatesTuple :: (Ord a) => [(a, a)] -> [(a, a)]
removeDuplicatesTuple xs = [(a, b) | i <- [0..length sortedPairs - 1],
                               let (a, b) = sortedPairs !! i,
                               not (elem (a, b) (take i sortedPairs))]
    where sortedPairs = [(min a b, max a b) | (a, b) <- xs]

removeDuplicatesString :: (Ord a) => [[a]] -> [[a]]
removeDuplicatesString xs = [a | i <- [0..length sortedPairs - 1],
                               let a = sortedPairs !! i,
                               not (elem a (take i sortedPairs))]
    where sortedPairs = [a | a <- xs]



fullHouses:: (Ord a) => [[a]]
fullHouses = removeDuplicatesString [[elem1, elem2, elem3, elem4, elem5]| elem1 <- baralho, elem2 <- baralho, elem3 <- baralho, elem4 <- baralho, elem5 <- baralho, fst (half elem1) == fst (half elem2), fst (half elem2) == fst (half elem3), fst (half elem4) == fst (half elem5), fst (half elem1) /= fst (half elem4)]


valueHandAux [card1, card2] = [ card1Value+card2Value | [card1Value, card2Value] <- [ [card1Values, card2Values] | card1Values <- cardValue(card1), card2Values <- cardValue(card2)]]

valueHand [card1, card2] = [value | value <- valueHandAux [card1, card2], not (value `elem` valueHandAux [card1, card2])]

--combinacoesBlackjackAux :: Int -> [(String, String)]
--combinacoesBlackjackAux points = [ (card1, card2) | card1 <- baralho, card2 <- baralho, (cardValue card1) + (cardValue card2) == [points], card1 /= card2]

main = do
  print (baralho)
  print ([(card, cardValue card) | card <- baralho])
  let hand = ["AS", "AH"]
  print (valueHandAux hand)
  print (valueHand hand)
  --print (valueHand ["AS", "AH"])

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
