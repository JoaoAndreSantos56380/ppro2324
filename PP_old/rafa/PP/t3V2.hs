{- 
Trabalho 3 PP 2022/23
Guilherme Marques - fc55472
Rafael Ferreira - fc57544
O objetivo deste trabalho é desenvolver um módulo para jogar aos labirintos.
-}
module Root.Src.Labirintos
(inicializa
, jogador
, chaves
, terminado
, move
, toLower
, sort
, EstadoJogo
) where

import Data.Char (toLower)
import Data.List (sort)

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
data EstadoJogo = EstadoJogo { lab :: [String]
, posicao :: (Int, Int)
, posicaoFinal :: (Int, Int)
, chavesCapturadas :: String
, nome :: String } 
 
{- Função que recebe um labirinto válido e devolve o estado inicial do jogo nesse labirinto -}
inicializa :: [String] -> EstadoJogo
inicializa xs = EstadoJogo { lab = xs
                            , posicao = (posicaoCaracter xs 'S')
                            , posicaoFinal = (posicaoCaracter xs 'F')
                            , chavesCapturadas = ""
                            , nome = "ej1" }

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

{-Função que recebe um estado de um jogo e devolve a posição atual do jogador -}
jogador :: EstadoJogo -> (Int, Int)
jogador = posicao

{- Função auxiliar que recebe o nome de um estado de um jogo e uma lista 
dos estados do jogo, e devolve o estado do jogo com o nome correspondente -}
{- encontraNome :: String -> [EstadoJogo] -> EstadoJogo
encontraNome _ [] = inicializa []                        
encontraNome x (y:ys) = if nome y == x then y else encontraNome x ys -}

{- Função que recebe um estado de um jogo e devolve 
a lista das chaves já adquiridas pelo jogador -}    
chaves :: EstadoJogo -> String
chaves = chavesCapturadas

{- Função auxiliar que recebe um labirinto valido e as coordenadas 
de uma posição no labirinto, e devolve o caracter dessas coordenadas -}
procurar :: [String] -> (Int, Int) -> Char
procurar xs (linha, coluna) 
    | linha > tamanhoLab || linha < 0 || coluna > tamanhoLab || coluna < 0 = '*' -- Posição inexistente do labirinto
    | otherwise = (xs !! linha) !! coluna
    where   tamanhoLab = length xs - 1

{- Função que recebe um estado de um jogo e indica se o jogador já atingiu a posição final -}
terminado :: EstadoJogo -> Bool
terminado estadoJogo = posicao estadoJogo == posicaoFinal estadoJogo

{- Mostra o labirinto, colocando um 'P' na posição atual onde o jogador se encontra.
Para além disso, mostra ainda a lista de chaves já adquiridas pelo jogador -}
instance Show EstadoJogo where
    show (EstadoJogo lab1 posicao1 _ chavesCapturadas1 _) =
        mostraLab ++ "chaves: " ++ sort chavesCapturadas1
        where mostraLab = foldr (\x acc -> x ++ "\n" ++ acc) "" (colocaChar lab1 posicao1 'P')

{- Função auxiliar que recebe um labirinto, uma posição
e um caracter, e coloca esse caracter nessa posição do labirinto -}
colocaChar :: [String] -> (Int, Int) -> Char -> [String]
colocaChar zs (x, y) c = foldr (\z acc -> if length zs - 1 - length acc == x then substitui (length zs - 1) z : acc else z : acc) [] zs
                        where substitui n a = foldr (\b acc -> if n - length acc == y then c : acc else b : acc) "" a

{--}
move :: EstadoJogo -> String -> EstadoJogo
move estadoJogo [] = estadoJogo
move estadoJogo (x:xs)
    | mov == '*' = move (movimentoParede estadoJogo cMov) xs
    | mov == 'S' || mov == ' ' = move (movimentoBase estadoJogo cMov) xs -- movimenta normal
    | mov == 'a' || mov == 'b' || mov == 'c' = move (movimentoChave estadoJogo (verMovimento x (posicao estadoJogo) (lab estadoJogo)) cMov) xs -- adicionar chaves
    | mov == 'A' || mov == 'B' || mov == 'C' = move (movimentoPorta x) xs -- ver se tem a chave, caso n tenha n movimenta
    | mov == 'F' = move (movimentoBase estadoJogo cMov) xs -- terminado retorna true
    | mov == '@' = move (movimentoPortal x estadoJogo cMov) xs -- portal
    | otherwise = undefined
    where mov = verMovimento x (posicao estadoJogo) (lab estadoJogo)
          cMov = movCoordenadas x (posicao estadoJogo)
          movimentoPorta c = if toLower (verMovimento c (posicao estadoJogo) (lab estadoJogo)) `elem` chavesCapturadas estadoJogo then movimentoBase estadoJogo (movCoordenadas c (posicao estadoJogo))
                             else movimentoParede estadoJogo cMov

verMovimento :: Char -> (Int, Int) -> [String] -> Char
verMovimento 'u' (x, y) lab' = procurar lab' (x - 1, y)
verMovimento 'l' (x, y) lab' = procurar lab' (x, y - 1)
verMovimento 'r' (x, y) lab' = procurar lab' (x, y + 1)
verMovimento 'd' (x, y) lab' = procurar lab' (x + 1, y)
verMovimento _ (x, y) lab' = procurar lab' (x, y)

movCoordenadas :: Char -> (Int, Int) -> (Int, Int)
movCoordenadas 'u' (x, y) = (x - 1, y)
movCoordenadas 'l' (x, y) = (x, y - 1)
movCoordenadas 'r' (x, y) = (x, y + 1)
movCoordenadas 'd' (x, y) = (x + 1, y)
movCoordenadas _ (x, y) = (x, y)

incrementaNome :: String -> String
incrementaNome [] = []
incrementaNome [x] = [x]
incrementaNome (_:_:xs) = "ej" ++ show (read xs + 1)

movimentoBase :: EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoBase estadoJogo pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
, posicao = pos
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = chavesCapturadas estadoJogo
, nome = incrementaNome (nome estadoJogo)}

movimentoChave :: EstadoJogo -> Char -> (Int, Int) -> EstadoJogo
movimentoChave estadoJogo c pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
, posicao = pos
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = c : chavesCapturadas estadoJogo
, nome = incrementaNome (nome estadoJogo)}

movimentoParede :: EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoParede estadoJogo pos = EstadoJogo { lab = lab estadoJogo
          , posicao = posicao estadoJogo
          , posicaoFinal = posicaoFinal estadoJogo
          , chavesCapturadas = chavesCapturadas estadoJogo
          , nome = incrementaNome (nome estadoJogo)}


movimentoPortal :: Char -> EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoPortal x estadoJogo pos = EstadoJogo { lab = verUltimoMov (lab estadoJogo) pos
          , posicao = if portais !! 0 == pos
                      then portais !! 1 else portais !! 0
          , posicaoFinal = posicaoFinal estadoJogo
          , chavesCapturadas = chavesCapturadas estadoJogo
          , nome = incrementaNome (nome estadoJogo)}
          where portais = findPortal (lab estadoJogo) 0

--No caso de haver apenas 1 portal no ultimo movimento realizado significa que estamos num deles, ou seja,
--vamos coloca um portal onde estamos e executamos o proximo movimento com o labirinto atualizado


verUltimoMov :: [String] -> (Int, Int) -> [String]
verUltimoMov labirinto pos
        | snd (posicaoCaracter labirinto 'S') == (-1) = colocaChar (colocaChar labirinto posP 'S') pos 'P'
        | length (findPortal labirinto 0) == 1 = colocaChar (colocaChar labirinto posP '@') pos 'P'
        | pos `elem` portais = colocaChar (colocaChar labirinto posP ' ') posPortal 'P'
        | otherwise = colocaChar (colocaChar labirinto posP ' ') pos 'P'
        where posP = posicaoCaracter labirinto 'P'
              posPortal = if head portais == pos then portais !! 1 else head portais
              portais = findPortal labirinto 0


{- Função que recebe um labirinto válido e 
devolva as coordenadas de um portal (caracter '@') nesse labirinto -}
findPortal :: [String] -> Int -> [(Int, Int)]
findPortal [] _ = []
findPortal (x:xs) linha
    | '@' `elem` x = (findPortalAux x linha 0) ++ (findPortal xs (linha + 1))
    | otherwise = (findPortal xs (linha + 1))

findPortalAux :: String -> Int -> Int -> [(Int, Int)]
findPortalAux [] _ _ = []
findPortalAux (x:xs) linha coluna
    | x == '@' = (linha, coluna) : findPortalAux xs linha (coluna + 1)
    | otherwise = findPortalAux xs linha (coluna + 1)


lab4 = ["*******","*c B S*","***** *","*a*F* *","*C*A* *","*b    *","*******"]
lab5 = ["*******","*  @  *","* ***B*","*a*S*F*","*** ***","*bA  @*","*******"]
lab6 = ["******","*@   *","*  S *","*   b*","*FAa@*","******"]
lab7 = ["********","*  @   *","* ****B*","* **** *","*a**S*F*","***  ***","*bA   @*","********"]
lab2 = ["*****","*S a*","*A***","*  F*","*****"]
