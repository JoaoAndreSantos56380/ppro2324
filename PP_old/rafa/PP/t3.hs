{- 
Trabalho 3 PP 2022/23
Guilherme Marques - fc55472
Rafael Ferreira - fc57544
O objetivo deste trabalho é desenvolver um módulo para jogar aos labirintos.
-}

module Labirintos  
(inicializa
, jogador
, chaves
, terminado
, move
, EstadoJogo
) where

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
data EstadoJogo = EstadoJogo { lab :: [String]
, posicao :: (Int, Int)
, posicaoFinal :: (Int, Int)
, chavesCapturadas :: String
, nome :: String
, listaEstados :: [EstadoJogo]
} 
 
{- Função que recebe um labirinto válido e devolve o estado inicial do jogo nesse labirinto -}
inicializa :: [String] -> EstadoJogo
inicializa xs = EstadoJogo { lab = xs
                            , posicao = (posicaoCaracter xs 'S')
                            , posicaoFinal = (posicaoCaracter xs 'F')
                            , chavesCapturadas = ""
                            , nome = "ej1"
                            , listaEstados = [] }

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
encontraNome :: String -> [EstadoJogo] -> EstadoJogo
encontraNome _ [] = inicializa []                        
encontraNome x (y:ys) = if nome y == x then y else encontraNome x ys

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
    show (EstadoJogo lab1 posicao1 _ chavesCapturadas1 nome1 _) =
        nome1 ++ "\n" ++ mostraLab ++ "chaves: " ++ chavesCapturadas1
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
    | mov == '*' = parede
    | mov == 'S' || mov == ' ' = move (movimentoBase estadoJogo (movCoordenadas x (posicao estadoJogo))) xs -- movimenta normal
    | mov == 'a' || mov == 'b' || mov == 'c' = move (movimentoChave estadoJogo x (movCoordenadas x (posicao estadoJogo))) xs -- adicionar chaves
    | mov == 'A' || mov == 'B' || mov == 'B' = estadoJogo -- ver se tem a chave, caso n tenha n movimenta
    | mov == 'F' = move (movimentoBase estadoJogo (movCoordenadas x (posicao estadoJogo))) xs -- termiando retorna true
    | mov == '@' = estadoJogo -- portal
    | otherwise = undefined
    where mov = verMovimento x (posicao estadoJogo) (lab estadoJogo)
          parede = EstadoJogo { lab = lab estadoJogo
          , posicao = posicao estadoJogo
          , posicaoFinal = posicaoFinal estadoJogo
          , chavesCapturadas = chavesCapturadas estadoJogo
          , nome = incrementaNome (nome estadoJogo)
          , listaEstados = estadoJogo : listaEstados estadoJogo }
          

verMovimento :: Char -> (Int, Int) -> [String] -> Char
verMovimento 'u' (x, y) lab = procurar lab (x - 1, y)
verMovimento 'l' (x, y) lab = procurar lab (x, y - 1)
verMovimento 'r' (x, y) lab = procurar lab (x, y + 1)
verMovimento 'd' (x, y) lab = procurar lab (x + 1, y)
verMovimento _ (x, y) lab = procurar lab (x, y)

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
movimentoBase estadoJogo (x, y) = EstadoJogo { lab = colocaChar (lab estadoJogo) (posicao estadoJogo) ' '
, posicao = (x, y)
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = chavesCapturadas estadoJogo
, nome = incrementaNome (nome estadoJogo)
, listaEstados = estadoJogo : listaEstados estadoJogo }

movimentoChave :: EstadoJogo -> Char -> (Int, Int) -> EstadoJogo
movimentoChave estadoJogo c (x, y) = EstadoJogo { lab = colocaChar (lab estadoJogo) (posicao estadoJogo) ' '
, posicao = (x, y)
, posicaoFinal = posicaoFinal estadoJogo
, chavesCapturadas = c : chavesCapturadas estadoJogo
, nome = incrementaNome (nome estadoJogo)
, listaEstados = estadoJogo : listaEstados estadoJogo }



lab4 = ["******","*F   *","**** *","***  *","*S a *","******"]
lab5 = ["******","*S   *","**** *","***  *","*F   *","******"]
lab6 = ["******","*    *","*  S *","*    *","*F ab*","******"]