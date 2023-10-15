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

-- Combinações Black Jack
combinacoesBlackjack :: Int -> [(String, String)]
combinacoesBlackjack points = removeDuplicatesTuple [ (card1, card2) | card1 <- baralho, card2 <- baralho, value1 <- (cardValue card1), value2 <- (cardValue card2), (value1 + value2) == points, card1 /= card2 ]

removeDuplicatesTuple :: (Ord a) => [(a, a)] -> [(a, a)]
removeDuplicatesTuple xs = [(a, b) | i <- [0..length sortedPairs - 1], let (a, b) = sortedPairs !! i, not ((a, b) `elem` (take i sortedPairs))]
    where sortedPairs = [(min a b, max a b) | (a, b) <- xs]


-- Full Houses
fullHouses = removeFirstThreeDuplicates ([ (card1, card2, card3, card4, card5) | card1 <- baralho, card2 <- baralho, card3 <- baralho, card4 <- baralho, card5 <- baralho, head card1 == head card2 && head card1 == head card3 && head card1 /= head card4 && head card4 == head card5 && tail card1 /= tail card2 && tail card1 /= tail card3 && tail card2 /= tail card3 && tail card4 /= tail card5 ])

removeFirstThreeDuplicates xs = [[a, b, c, d, e] | i <- [0..length sortedQuintuple - 1],
                               let (a, b, c, d, e) = sortedQuintuple !! i,
                               not ((a, b, c, d, e) `elem` (take i sortedQuintuple))]
    where sortedQuintuple = [(minimum [a, b, c], minimum [x | x <- [a, b, c], x /= minimum [a, b, c]], maximum [a, b, c], min d e, max d e) | (a, b, c, d, e) <- xs]

{- main = do
  let full = fullHouses
  print (take 10 full)
  print (length full) -}
