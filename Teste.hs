import Data.List

-- Assuming baralho is your deck of cards
baralho :: [String]
baralho = [[rank, suit] | rank <- "23456789TJQKA", suit <- "SHDC"]


-- Function to generate all possible pairs from a list of cards
geraPares :: [String] -> [(String, String)]
geraPares cartas = [(c1, c2) | c1 <- cartas, c2 <- cartas, c1 < c2]

-- Function to generate all possible trios from a list of cards
geraTrios :: [String] -> [(String, String, String)]
geraTrios cartas = [(c1, c2, c3) | c1 <- cartas, c2 <- cartas, c3 <- cartas, c1 < c2, c2 < c3]

-- Function to generate all Full House hands
fullHouses :: [[String]]
fullHouses = [ [t1, t2, t3, p1, p2] | (t1, t2, t3) <- trios, (p1, p2) <- pares, t1 /= p1, t1 /= p2, t2 /= p1, t2 /= p2, t3 /= p1, t3 /= p2]
    where
        -- Get all possible trios and pairs
        trios = geraTrios baralho
        pares = geraPares (baralho \\ concatMap (\(c1, c2, c3) -> [c1, c2, c3]) trios)

main :: IO ()
main = print $ take 5 fullHouses
