{-
Princípios de Programação 2022/2023
Trabalho 4 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
t4_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

$ stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes três tipos de instruções:

$ ./Main [ficheiro] -- carrega um ficheiro para jogar
$ ./Main            -- carrega o ficheiro default.map
$ ./Main -t         -- corre os testes
-}

--ha um bug no default.map quando se faz o move ddll. o P desaparece.

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
module Main
(main
, readFromInput
, labirintoFicheiro
) where

import System.IO
import System.Environment
import Data.Char (toLower)
import Data.List (sort)
import System.Directory
import Test.QuickCheck
import System.Random
--import Labirintos
--import Testes

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
data EstadoJogo = EstadoJogo { lab :: [String]
, posicao :: (Int, Int)
, posicaoFinal :: (Int, Int)
, chavesCapturadas :: String }


{- Função a ser executada pela linha de comandos através de um comando-}
main :: IO ()
main =  do
        x <- getArgs
        --caso sejam passados mais que 1 argumento ou o ficheiro nao exista e nao seja o default
        fileExists <- doesFileExist (if null x then "default.map" else head x)
        if (not (null x) && head x == "-t") then testes else do
        if not fileExists || length x > 1 then putStrLn texto else do
        handle <- openFile (if null x then "default.map" else head x) ReadMode
        posicaoInicial <- hGetLine handle
        chaves' <- hGetLine handle
        contents <- hGetContents handle
        let contentsList = lines contents
        --inicializar o novo jogo
        let estadoJogo = jaInicializa (contentsList) (read posicaoInicial :: (Int, Int)) (posicaoCaracter (contentsList) ('F')) (chaves')
        putStr $ unlines (colocaChar contentsList (posicao estadoJogo) 'P')
        putStrLn ("chaves: " ++ chavesCapturadas estadoJogo)
        instruction <- getLine
        readFromInput (words instruction) estadoJogo chaves'
        hClose handle
        return ()
        where texto = "Utilização: " ++ "\n" ++ " ./Main [ficheiro] -- carrega um ficheiro para jogar" ++ "\n"
                     ++ " ./Main            -- carrega o ficheiro default.map" ++ "\n" ++ " ./Main -t         -- corre os testes"

{- Função auxiliar que le interações do utilizador com o programa até este querer sair do programa -}
readFromInput :: [String] -> EstadoJogo -> String -> IO ()
readFromInput x estadoJogo chaves' = 
    do
        case head x of  "exit" -> return ()
                        "move" -> do  
                                    let estadoJogo' = move estadoJogo (x !! 1)
                                    putStr $ unlines (lab estadoJogo')
                                    putStrLn ("chaves: " ++ chavesCapturadas estadoJogo')
                                    info <- getLine
                                    readFromInput (words info) estadoJogo' (chavesCapturadas estadoJogo')
                        "load" -> do
                                    handle <- openFile (x !! 1) ReadMode
                                    posicaoInicial <- hGetLine handle
                                    chaves'' <- hGetLine handle
                                    contents <- hGetContents handle
                                    let contentsList = lines contents
                                    let estadoJogo' = jaInicializa (contentsList) (read posicaoInicial :: (Int, Int)) (posicaoCaracter (contentsList) ('F')) (chaves'')
                                    putStr $ unlines (colocaChar contentsList (posicao estadoJogo') 'P')
                                    putStrLn ("chaves: " ++ chavesCapturadas estadoJogo')
                                    info <- getLine
                                    readFromInput (words info) estadoJogo' chaves''
                        "save" -> do
                                    writeFile (x !! 1) (posicaoJogador ++ "\n" ++ (chaves') ++ "\n" ++ unlines (labirintoFicheiro (jogador estadoJogo) (lab estadoJogo)))
                                    putStr $ unlines $ (colocaChar (lab estadoJogo) (posicao estadoJogo) 'P')
                                    putStrLn ("chaves: " ++ chaves')
                                    info <- getLine
                                    readFromInput (words info) estadoJogo chaves'
                                    where posicaoJogador = "(" ++ (show $ fst $ jogador estadoJogo) ++ "," ++ (show $ snd $ jogador estadoJogo) ++ ")"
                        [] -> putStrLn "Error"
                        [_] -> putStrLn "Error"
                        (_:_:_) -> putStrLn "Error"

{- Função auxiliar que retira o 'P' de um labirinto e devolve-o no estado atualizado -}
labirintoFicheiro :: (Int, Int) -> [String] -> [String]
labirintoFicheiro x xs
    | snd (posicaoCaracter xs 'S') == (-1) = colocaChar xs x 'S'
    | length (findChar xs 0 '@') == 1 = colocaChar xs x '@'
    | otherwise = colocaChar xs x ' '



--Testes

testes :: IO ()
testes = do
        quickCheck prop_dimensions
        quickCheck prop_limits
        quickCheck prop_keys
        quickCheck prop_doors


prop_keys :: EstadoJogo -> Movimentos -> Movimentos -> Bool
prop_keys estadoJogo mov mov2 = length (chaves (move estadoJogo (movToString mov))) <= length (chaves (move (move estadoJogo (movToString mov)) (movToString mov2)))

prop_dimensions :: EstadoJogo -> Movimentos -> Movimentos -> Bool
prop_dimensions estadoJogo mov mov2 = (length labMov2 == length labMov) && (length (labMov2 !! 1)) == (length (labMov !! 1))
                                    where labMov = (lab (move estadoJogo (movToString mov)))
                                          labMov2 = (lab (move (move estadoJogo (movToString mov)) (movToString mov2)))

prop_limits :: EstadoJogo -> Movimentos -> Movimentos -> Bool
prop_limits estadoJogo mov mov2 = (fst (posicaoAtual) > 0) && (fst (posicaoAtual) < (length (lab (movimento))) - 1) && (snd (posicaoAtual) > 0) && (snd (posicaoAtual) < (length (lab (movimento) !! 0)) - 1)
                                    && (fst (posicaoAtual') > 0) && (fst (posicaoAtual') < (length (lab (movimento))) - 1) && (snd (posicaoAtual') > 0) && (snd (posicaoAtual') < (length (lab (movimento) !! 0)) - 1)
                                where   posicaoAtual = posicao movimento
                                        posicaoAtual' = posicao (move movimento (movToString mov2))
                                        movimento = move estadoJogo (movToString mov)

prop_doors :: EstadoJogo -> Movimentos -> Movimentos -> Bool
prop_doors estadoJogo mov mov2 = length (findDoors (lab (move estadoJogo (movToString mov))) "ABC") >= length (findDoors (lab (move (move estadoJogo (movToString mov)) (movToString mov2))) "ABC")

findDoors :: [String] -> String -> [(Int, Int)]
findDoors _ "" = []
findDoors lab (x:xs)
    | snd (posicaoCaracter lab x) == (-1) = findDoors lab xs
    | otherwise = (posicaoCaracter lab x) : findDoors lab xs


--teste que verifica se o nr de portais esta correto
prop_valid_portals :: EstadoJogo -> Movimentos -> Bool
prop_valid_portals estadoJogo mov = findPortal (lab estadoJogo) 0 >= findPortal (lab (move estadoJogo (movToString mov))) 0


--propriedade que verifica se existe sempre 1 e um so jogador no labirinto
prop_player :: EstadoJogo -> Movimentos -> Movimentos -> Bool
prop_player estadoJogo mov mov2 = (length (findPlayer (lab estadoJogo)) == length (findPlayer (lab (move estadoJogo (movToString mov)))))
                                == (length (findPlayer (lab (move (move estadoJogo (movToString mov)) (movToString mov2)))) == 1)


findPlayer :: [String] -> [(Int, Int)]
findPlayer lab
    | snd (posicaoCaracter lab 'P') == (-1) = []
    | otherwise = posicaoCaracter lab 'P' : findPlayer lab

{- Teste que verifica a propriedade: caso executemos uma sequencia de movimentos ao executar 
o seu inverso temos de acabar na mesma posicao onde ininciamos o movimento incial. -}
prop_reverse_move :: EstadoJogo -> Movimentos -> Bool
prop_reverse_move estadoJogo mov = (length (findChar (lab (move estadoJogo (movToString mov))) 0 '@') == 1) ||
                                   posicao estadoJogo == posicao (move (move estadoJogo (movToString mov)) reversedMove)
                                    where reversedMove = reverseMove (filterString (movToString mov) (verificaMov estadoJogo (movToString mov) 0) 0)


verificaMov :: EstadoJogo -> String -> Int -> [Int]
verificaMov _ [] _ = []
verificaMov estadoJogo (x:xs) count
    | posicao mov == posicao estadoJogo = count : (verificaMov mov xs (count + 1))
    | otherwise = (verificaMov mov xs (count + 1))
    where mov = move estadoJogo [x]


filterString :: String -> [Int] -> Int -> String
filterString [] _ _ = []
filterString x [] _ = x
filterString (x:xs) (y:ys) count
    | count == y = filterString xs ys (count + 1)
    | otherwise = x : filterString xs (y:ys) (count + 1)

{- Função auxiliar que mapeia o complementar de cada movimento a String passada como argumento-}
reverseMove :: String -> String
reverseMove xs = map reverseMoveAux (reverse xs)

{- Função auxiliar que dado um caracter devolve o seu inverso\complementar -}
reverseMoveAux :: Char -> Char
reverseMoveAux c = case c of    'd' -> 'u'
                                'u' -> 'd'
                                'r' -> 'l'
                                'l' -> 'r'
                                _ -> ' '



newtype Movimentos = Movimentos String deriving Show

movToString :: Movimentos -> String
movToString (Movimentos x) = x


instance Arbitrary Movimentos where
        arbitrary = do
                    n <- choose (0, 10) :: Gen Int
                    m <- vectorOf n $ (elements "udrl")
                    return (Movimentos m)

-- EstadoJogo aleatorios
instance Arbitrary EstadoJogo where
        arbitrary = do
                    linhas <- choose (3,10) :: Gen Int
                    colunas <- choose (3,10) :: Gen Int
                    total <- choose (0, 100) :: Gen Int
                    let random = randomizeInt (linhas * colunas) [] (total)
                    portal <- choose (0, 1) :: Gen Int
                    portas <- choose (0, 7) :: Gen Int
                    chaves <- choose (0, 7) :: Gen Int
                    paredes <- choose (0, (linhas * colunas) - 10) :: Gen Int
                    let caracteresLab = listaCaracteres portal portas chaves paredes (linhas * colunas)
                    let preenchido = preencheLab random caracteresLab
                    let colocaN = lines (poeN linhas preenchido 0)
                    let comParedes = colocaTodasParedes colocaN []
                    return (inicializa comParedes)


preencheLab :: [Int] -> String -> String
preencheLab [] _ = []
preencheLab (x:xs) ys = (ys !! x) : preencheLab xs ys

--recebe o nr de linhas que é suposto o lab ter, lembrando que os labirintos podem ser 5x7, 9x3, etc
poeN :: Int -> String -> Int -> String
poeN  _ "" _ = ""
poeN x (y:ys) count
    | (count /= 0 && (mod (count + 1) x == 0)) = y : "\n" ++ poeN x ys (count + 1)
    | otherwise = y : poeN x ys (count + 1)

--coloca paredes, recebe array vazio como segundo argumento
colocaTodasParedes :: [String] -> [String] -> [String]
colocaTodasParedes [] [] = []
colocaTodasParedes labirinto labirintoComParedes
    | length labirintoComParedes == 0 = colocaTodasParedes labirinto (colocaParedes parede : labirintoComParedes)
    | length labirintoComParedes == (length labirinto + 1) = colocaParedes parede : labirintoComParedes
    | otherwise = colocaTodasParedes labirinto (colocaParedesLab labirinto ++ labirintoComParedes)
    where   parede = criarParede (length (labirinto !! 0)) 0 ""
            colocaParedes x = "*" ++ x ++ "*"
            colocaParedesLab [] = []
            colocaParedesLab (x:xs) = ("*" ++ x ++ "*") : colocaParedesLab xs
            criarParede x y parede' = if x == y then parede' else criarParede (x) (y + 1) ("*" ++ parede')

listaCaracteres :: Int -> Int -> Int -> Int -> Int -> String
listaCaracteres portal portas nChaves paredes tamanho = do
            let a = 'S' : 'F' : portaisAleatorios portal ++ portasAleatorias portas ++ chavesAleatorias nChaves ++ preencheComChar paredes 0 '*'
            let b = tamanho - length a --tamanho restante da string sem caracteres ocupados
            a ++ preencheComChar b 0 ' '

preencheComChar :: Int -> Int -> Char -> String
preencheComChar 0 _ _ = ""
preencheComChar x count c
    |  x /= count = c : preencheComChar x (count + 1) c
    | otherwise = c : preencheComChar 0 (count + 1) c

--funcao para escolher portais aleatorios (0 ou 2)
portaisAleatorios :: Int -> String
portaisAleatorios m = case m of
                        0 -> ""
                        _ -> "@@"

--funcao para escolher portas aleatorios (0,1,2...7; todas as combinacoes possiveis)
portasAleatorias :: Int -> String
portasAleatorias m = case m of
                        0 -> ""
                        1 -> "A"
                        2 -> "B"
                        3 -> "C"
                        4 -> "AB"
                        5 -> "BC"
                        6 -> "AC"
                        _ -> "ABC"

--funcao para escolher portas aleatorios (0,1,2...3; todas as combinacoes possiveis)
chavesAleatorias :: Int -> String
chavesAleatorias m = case m of
                        0 -> ""
                        1 -> "a"
                        2 -> "b"
                        3 -> "c"
                        4 -> "ab"
                        5 -> "bc"
                        6 -> "ac"
                        _ -> "abc"

insertChar :: String -> Int -> Int -> Char -> String
insertChar [] _ _ _ = []
insertChar (x:xs) y count c
    | count == y = c : insertChar xs y (count + 1) c
    | otherwise = x : insertChar xs y (count + 1) c

randomizeInt :: Int -> [Int] -> Int -> [Int]
randomizeInt numero repeticoes seed
  | length repeticoes == numero = repeticoes
  | elem numeroAleatorio repeticoes = randomizeInt numero repeticoes (seed + 1)
  | otherwise = randomizeInt numero (numeroAleatorio : repeticoes) (seed + 1)
  where
      numeroAleatorio = fst $ randomR (0, (numero - 1)) (mkStdGen seed)

preencheVazia :: Int -> Int -> String
preencheVazia 0 _ = ""
preencheVazia x count
    |  x /= count = " " ++ preencheVazia x (count + 1)
    | otherwise = " " ++ preencheVazia 0 (count + 1)




----------------------------------------Labirintos.hs \ Trabalho 3---------------------------------------------

{- Função que recebe um labirinto válido e devolve o estado inicial do jogo nesse labirinto -}
inicializa :: [String] -> EstadoJogo
inicializa xs = EstadoJogo { lab = xs
                            , posicao = (posicaoCaracter xs 'S')
                            , posicaoFinal = (posicaoCaracter xs 'F')
                            , chavesCapturadas = "" }

{- Função que recebe um labirinto válido e devolve o estado inicial do jogo nesse labirinto -}
jaInicializa :: [String] -> (Int, Int) -> (Int, Int) -> String -> EstadoJogo
jaInicializa xs x y z = EstadoJogo { lab = xs
                            , posicao = x
                            , posicaoFinal = y
                            , chavesCapturadas = z}

{- Função auxiliar que recebe um labirinto válido, um caracter e
devolve as coordenadas desse caracter nesse labirinto -}
posicaoCaracter :: [String] -> Char -> (Int, Int)
posicaoCaracter xs car = (coluna, linha)
    where   coluna = (sum (scanl (\acc x -> if elem car x then acc - 1 else acc) (1) (xs))) - 1
            linha = encontrarLinha xs car

{- Função auxiliar que recebe um labirinto valido, um caracter e devolve 
a posição da linha onde o caracter se encontra nesse labirinto -}
encontrarLinha :: [String] -> Char -> Int
encontrarLinha [] _ = (-1)
encontrarLinha (x:xs) y = if elem y x then (sum (scanl (\acc pos -> if pos == y then acc - 1 else acc) (1) (x))) - 1 else encontrarLinha xs y

{- Função que recebe um estado de um jogo e devolve a posição atual do jogador -}
jogador :: EstadoJogo -> (Int, Int)
jogador xs = posicaoCaracter (lab xs) ('P')

{- Função que recebe um estado de um jogo e devolve 
a lista das chaves já adquiridas pelo jogador -}
chaves :: EstadoJogo -> String
chaves = chavesCapturadas

{- Função auxiliar que recebe um labirinto valido e as coordenadas 
de uma posição no labirinto, e devolve o caracter dessas coordenadas -}
procurar :: [String] -> (Int, Int) -> Char
procurar xs (linha, coluna) = (xs !! linha) !! coluna

{- Função que recebe um estado de um jogo e indica se o jogador já atingiu a posição final -}
terminado :: EstadoJogo -> Bool
terminado estadoJogo = posicao estadoJogo == posicaoFinal estadoJogo

{- Mostra o labirinto, colocando um 'P' na posição atual onde o jogador se encontra.
Para além disso, mostra ainda a lista de chaves já adquiridas pelo jogador -}
instance Show EstadoJogo where
    show (EstadoJogo lab1 posicao1 _ chavesCapturadas1) =
        mostraLab ++ "chaves: " ++ sort chavesCapturadas1
        where mostraLab = foldr (\x acc -> x ++ "\n" ++ acc) "" (colocaChar lab1 posicao1 'P')

{- Função auxiliar que recebe um labirinto, uma posição
e um caracter, e coloca esse caracter nessa posição do labirinto -}
colocaChar :: [String] -> (Int, Int) -> Char -> [String]
colocaChar zs (x, y) c = foldr (\z acc -> if length zs - 1 - length acc == x then substitui (length (head zs) - 1) z : acc else z : acc) [] zs
                        where substitui n a = foldr (\b acc -> if n - length acc == y then c : acc else b : acc) "" a

{- Função que recebe um estado de um jogo e uma sequência de movimentos, e devolve
o estado do jogo resultante -}
move :: EstadoJogo -> String -> EstadoJogo
move estadoJogo [] = estadoJogo
move estadoJogo (x:xs)
    | mov == '*' = move (movimentoParede estadoJogo) xs -- Parede
    | mov == 'S' || mov == ' ' = move (movimentoBase estadoJogo cMov) xs -- Movimento normal
    | mov == 'a' || mov == 'b' || mov == 'c' = move (movimentoChave estadoJogo (verMovimento x (posicao estadoJogo) (lab estadoJogo)) cMov) xs -- Chave
    | mov == 'A' || mov == 'B' || mov == 'C' = move (movimentoPorta x) xs -- Porta 
    | mov == 'F' = move (movimentoBase estadoJogo cMov) xs -- Final do jogo (terminado passa para true)
    | mov == '@' = move (movimentoPortal estadoJogo cMov) xs -- Portal
    | mov == 'P' = move (movimentoParede estadoJogo) xs
    | otherwise = estadoJogo
    where mov = verMovimento x (posicao estadoJogo) (lab estadoJogo)
          cMov = movCoordenadas x (posicao estadoJogo)
          movimentoPorta c = if toLower (verMovimento c (posicao estadoJogo) (lab estadoJogo)) `elem` chavesCapturadas estadoJogo then movimentoBase estadoJogo (movCoordenadas c (posicao estadoJogo))
                             else movimentoParede estadoJogo --Confirma se ja temos a chave da correspondente porta

{- Função auxiliar que devolve o caracter da posição do movimento realizado -}
verMovimento :: Char -> (Int, Int) -> [String] -> Char
verMovimento 'u' (x, y) lab' = procurar lab' (x - 1, y)
verMovimento 'l' (x, y) lab' = procurar lab' (x, y - 1)
verMovimento 'r' (x, y) lab' = procurar lab' (x, y + 1)
verMovimento 'd' (x, y) lab' = procurar lab' (x + 1, y)
verMovimento _ (x, y) lab' = procurar lab' (x, y)

{- Função auxiliar que devolve as coordenadas do movimento realizado -}
movCoordenadas :: Char -> (Int, Int) -> (Int, Int)
movCoordenadas 'u' (x, y) = (x - 1, y)
movCoordenadas 'l' (x, y) = (x, y - 1)
movCoordenadas 'r' (x, y) = (x, y + 1)
movCoordenadas 'd' (x, y) = (x + 1, y)
movCoordenadas _ (x, y) = (x, y)

{- Função auxiliar que executa o movimento correspondente a um espaço, atualizando o labirinto de acordo -}
movimentoBase :: EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoBase estadoJogo pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
, posicao = pos
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = chavesCapturadas estadoJogo}

{- Função auxiliar que executa o movimento correspondente a uma chave, atualizando o labirinto 
e lista de chaves de acordo -}
movimentoChave :: EstadoJogo -> Char -> (Int, Int) -> EstadoJogo
movimentoChave estadoJogo c pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
, posicao = pos
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = c : chavesCapturadas estadoJogo}

{- Função auxiliar que executa o movimento correspondente a uma parede.
Pelo nosso entender do trabalho uma tentativa de movimentaçao necessita de uma nova instancia 
do EstadoJogo, mesmo que permaneça tudo igual -}
movimentoParede :: EstadoJogo  -> EstadoJogo
movimentoParede estadoJogo = EstadoJogo { lab = verUltimoMov (lab estadoJogo) (posicao estadoJogo)
          , posicao = posicao estadoJogo
          , posicaoFinal = posicaoFinal estadoJogo
          , chavesCapturadas = chavesCapturadas estadoJogo}

{- Função auxiliar que executa o movimento correspondente a um portal e que caso estejamos a tentar
mover para um deles iremos automcaticamente parar ao outro, atualizando assim o labirinto de acordo-}
movimentoPortal :: EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoPortal estadoJogo pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
          , posicao = if (pos `elem` portais && (length portais == 1)) then posicao estadoJogo else
                      if head portais == pos
                      then portais !! 1 else head portais
          , posicaoFinal = posicaoFinal estadoJogo
          , chavesCapturadas = chavesCapturadas estadoJogo}
          where portais = findPortal (lab estadoJogo) 0

{- Função auxiliar que avalia o ultimo movimento e altera o labirinto consoante 
o movimento colocando sempre o caracter correspondente na posição correta.
No caso de haver apenas 1 portal no ultimo movimento realizado significa que estamos num deles, ou seja,
vamos coloca um portal onde estamos e executamos o proximo movimento com o labirinto atualizado. 
Seguimos esta logica com a posicao inicial, porque caso nao exista no labirinto significa que estamos nessa posição -}
verUltimoMov :: [String] -> (Int, Int) -> [String]
verUltimoMov labirinto pos
        | snd (posicaoCaracter labirinto 'S') == (-1) = colocaChar (colocaChar labirinto posP 'S') pos 'P'  -- Verifica se estamos na posicao inicial
        | length (findPortal labirinto 0) == 1 = colocaChar (colocaChar labirinto posP '@') pos 'P'  -- Verifica se estamos num portal
        | pos `elem` portais && (length portais == 1) = labirinto
        | pos `elem` portais = colocaChar (colocaChar labirinto posP ' ') posPortal 'P'  -- Verifica se o movimento que queremos executar é para um portal
        | otherwise = colocaChar (colocaChar labirinto posP ' ') pos 'P'  -- Coloca apenas um espaco na posicao onde estavamos caso nao seja nenhum dos casos anteriores
        where posP = posicaoCaracter labirinto 'P'
              posPortal = if length portais == 1 then posP else (if head portais == pos then portais !! 1 else head portais)
              portais = findPortal labirinto 0

{- Função auxiliar que recebe um labirinto e 
devolva as coordenadas de um portal (caracter '@') nesse labirinto -}
findPortal :: [String] -> Int -> [(Int, Int)]
findPortal [] _ = []
findPortal (x:xs) linha
    | '@' `elem` x = (findPortalAux x linha 0) ++ (findPortal xs (linha + 1))
    | otherwise = (findPortal xs (linha + 1))

{- Função auxiliar da função findPortal -}
findPortalAux :: String -> Int -> Int -> [(Int, Int)]
findPortalAux [] _ _ = []
findPortalAux (x:xs) linha coluna
    | x == '@' = (linha, coluna) : findPortalAux xs linha (coluna + 1)
    | otherwise = findPortalAux xs linha (coluna + 1)

{- Função auxiliar que recebe um labirinto e 
devolva as coordenadas de um portal (caracter '@') nesse labirinto -}
findChar :: [String] -> Int -> Char -> [(Int, Int)]
findChar [] _ _ = []
findChar (x:xs) linha c
    | c `elem` x = (findCharAux x linha 0 c) ++ (findChar xs (linha + 1) c)
    | otherwise = (findChar xs (linha + 1) c)

{- Função auxiliar da função findPortal -}
findCharAux :: String -> Int -> Int -> Char -> [(Int, Int)]
findCharAux [] _ _ _ = []
findCharAux (x:xs) linha coluna c
    | x == c = (linha, coluna) : findCharAux xs linha (coluna + 1) c
    | otherwise = findCharAux xs linha (coluna + 1) c

lab1 = ["SF","SF","SF","SF"]
lab4 = ["******","*F   *","**** *","* *  *","*S a *","*    *","******"]
lab5 = ["******","*S   *","**** *","***  *","*F   *","******"]
lab6 = ["******","*  Sa*","*   B*","*F Ab*","**  **","******"]
lab7 = ["*****","*Sc**","*F*@*","*@* *","*****"]
lab8 = ["********","*  @ @ *","* b    *","*a F SB*","********"]
lab9 = ["***********","*C  * *** *","**@  S  @**","*** *  F***","***********","***********","***********","***********"]
lab10 = lines (poeN 6 "******* a* *** *c** ******** **@   ** **S**  @ *****F*******" 0)