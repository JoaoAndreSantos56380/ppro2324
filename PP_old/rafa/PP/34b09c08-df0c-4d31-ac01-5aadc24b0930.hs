
contaCaracteres :: Char -> [String] -> Int
contaCaracteres x ys = length [w | z <- ys, w <- z, w == x]

labirintos5 :: [[String]]
labirintos5 = [["*****", x, y, z, "*****"]| x <- possiveis, y <- possiveis, z <- possiveis,
                  contaCaracteres 'S' [x, y, z] == 1, contaCaracteres 'F' [x, y, z] == 1]

possiveis :: [String]
possiveis = [['*', x, y, z, '*'] | x <- tipos, y <- tipos, z <- tipos,
            contaCaracteres 'S' [[x, y, z]] <= 1, contaCaracteres 'F' [[x, y, z]] <= 1]

tipos :: String
tipos = "SF* "

listaCaracteres :: Int -> Int -> Int -> Int -> Int -> String
listaCaracteres portal portas nChaves paredes tamanho = do
            let a = 'S' : 'F' : portaisAleatorios portal ++ portasAleatorias portas ++ chavesAleatorias nChaves ++ preencheComChar paredes 0 '*'
            let b = tamanho - length a --tamanho restante da string sem caracteres ocupados
            a ++ preencheComChar b 0 ' '


--funcao para escolher portais aleatorios (0 ou 2)
portaisAleatorios :: Int -> String
portaisAleatorios m = case m of
                        0 -> ""
                        _ -> "@@" 

--funcao para escolher portas aleatorios (0,1,2 ou 3; todas as combinacoes possiveis)
portasAleatorias :: Int -> String
portasAleatorias m = case m of
                        0 -> ""
                        1 -> "A"
                        2 -> "AB"
                        _ -> "ABC"

--funcao para escolher portas aleatorios (0,1,2 ou 3; todas as combinacoes possiveis)
chavesAleatorias :: Int -> String
chavesAleatorias m = case m of
                        0 -> ""
                        1 -> "a"
                        2 -> "ab"
                        _ -> "abc"


insertChar :: String -> Int -> Int -> Char -> String
insertChar [] _ _ _ = []
insertChar (x:xs) y count c
    | count == y = c : insertChar xs y (count + 1) c
    | otherwise = x : insertChar xs y (count + 1) c

 {-}   
randomizeInt :: Int -> [Int] -> Int -> [Int]
randomizeInt numero repeticoes seed
  | length repeticoes == numero = repeticoes
  | elem numeroAleatorio repeticoes = randomizeInt numero repeticoes (seed + 1)
  | otherwise = randomizeInt numero (numeroAleatorio : repeticoes) (seed + 1)
  where
      numeroAleatorio = fst $ randomR (0, (numero - 1)) (mkStdGen seed)
-}

preencheComChar :: Int -> Int -> Char -> String
preencheComChar 0 _ _ = ""
preencheComChar x count c 
    |  x /= count = c : preencheComChar x (count + 1) c
    | otherwise = c : preencheComChar 0 (count + 1) c