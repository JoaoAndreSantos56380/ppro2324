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

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
--module Main where

import Data.Char (toLower)
import Data.List (sort)
import System.Environment
import System.IO


main = do
  x <- getArgs
  handle <- openFile (if null x then "default.map" else head x) ReadMode
  posicaoInicial <- hGetLine handle
  chaves' <- hGetLine handle
  contents <- hGetContents handle
  --caso for default map; fazer o putStrLn
  putStr contents
  putStrLn ("chaves: " ++ chaves')
  instrucao <- getLine
  return ()

{- Import qualificado para lidar com situacoes especificas em listas e com chars -}

{- Tipo de dados que permite representar o jogo de um labirinto num dado momento -}
data EstadoJogo = EstadoJogo
  { lab :: [String],
    posicao :: (Int, Int),
    posicaoFinal :: (Int, Int),
    chavesCapturadas :: String
  }

{- Função que recebe um labirinto válido e devolve o estado inicial do jogo nesse labirinto -}
inicializa :: [String] -> EstadoJogo
inicializa xs =
  EstadoJogo
    { lab = xs,
      posicao = (posicaoCaracter xs 'S'),
      posicaoFinal = (posicaoCaracter xs 'F'),
      chavesCapturadas = ""
    }

{- Função auxiliar que recebe um labirinto válido, um caracter e
devolve as coordenadas desse caracter nesse labirinto -}
posicaoCaracter :: [String] -> Char -> (Int, Int)
posicaoCaracter xs car = (coluna, linha)
  where
    coluna = (sum (scanl (\acc x -> if elem car x then acc - 1 else acc) (1) (xs))) - 1
    linha = encontrarLinha xs car

{- Função auxiliar que recebe um labirinto valido, um caracter e devolve
a posição da linha onde o caracter se encontra nesse labirinto -}
encontrarLinha :: [String] -> Char -> Int
encontrarLinha [] _ = (-1)
encontrarLinha (x : xs) y = if elem y x then (sum (scanl (\acc pos -> if pos == y then acc - 1 else acc) (1) (x))) - 1 else encontrarLinha xs y

{- Função que recebe um estado de um jogo e devolve a posição atual do jogador -}
jogador :: EstadoJogo -> (Int, Int)
jogador = posicao

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
  where
    tamanhoLab = length xs - 1

{- Função que recebe um estado de um jogo e indica se o jogador já atingiu a posição final -}
terminado :: EstadoJogo -> Bool
terminado estadoJogo = posicao estadoJogo == posicaoFinal estadoJogo

{- Mostra o labirinto, colocando um 'P' na posição atual onde o jogador se encontra.
Para além disso, mostra ainda a lista de chaves já adquiridas pelo jogador -}
instance Show EstadoJogo where
  show (EstadoJogo lab1 posicao1 _ chavesCapturadas1) =
    mostraLab ++ "chaves: " ++ sort chavesCapturadas1
    where
      mostraLab = foldr (\x acc -> x ++ "\n" ++ acc) "" (colocaChar lab1 posicao1 'P')

{- Função auxiliar que recebe um labirinto, uma posição
e um caracter, e coloca esse caracter nessa posição do labirinto -}
colocaChar :: [String] -> (Int, Int) -> Char -> [String]
colocaChar zs (x, y) c = foldr (\z acc -> if length zs - 1 - length acc == x then substitui (length zs - 1) z : acc else z : acc) [] zs
  where
    substitui n a = foldr (\b acc -> if n - length acc == y then c : acc else b : acc) "" a

{- Função que recebe um estado de um jogo e uma sequência de movimentos, e devolve
o estado do jogo resultante -}
move :: EstadoJogo -> String -> EstadoJogo
move estadoJogo [] = estadoJogo
move estadoJogo (x : xs)
  | mov == '*' = move (movimentoParede estadoJogo) xs -- Parede
  | mov == 'S' || mov == ' ' = move (movimentoBase estadoJogo cMov) xs -- Movimento normal
  | mov == 'a' || mov == 'b' || mov == 'c' = move (movimentoChave estadoJogo (verMovimento x (posicao estadoJogo) (lab estadoJogo)) cMov) xs -- Chave
  | mov == 'A' || mov == 'B' || mov == 'C' = move (movimentoPorta x) xs -- Porta
  | mov == 'F' = move (movimentoBase estadoJogo cMov) xs -- Final do jogo (terminado passa para true)
  | mov == '@' = move (movimentoPortal estadoJogo cMov) xs -- Portal
  | otherwise = error "Labirinto invalido!"
  where
    mov = verMovimento x (posicao estadoJogo) (lab estadoJogo)
    cMov = movCoordenadas x (posicao estadoJogo)
    movimentoPorta c =
      if toLower (verMovimento c (posicao estadoJogo) (lab estadoJogo)) `elem` chavesCapturadas estadoJogo
        then movimentoBase estadoJogo (movCoordenadas c (posicao estadoJogo))
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
movimentoBase estadoJogo pos =
  EstadoJogo
    { lab = verUltimoMov (lab estadoJogo) pos,
      posicao = pos,
      posicaoFinal = posicaoFinal estadoJogo,
      chavesCapturadas = chavesCapturadas estadoJogo
    }

{- Função auxiliar que executa o movimento correspondente a uma chave, atualizando o labirinto
e lista de chaves de acordo -}
movimentoChave :: EstadoJogo -> Char -> (Int, Int) -> EstadoJogo
movimentoChave estadoJogo c pos =
  EstadoJogo
    { lab = verUltimoMov (lab estadoJogo) pos,
      posicao = pos,
      posicaoFinal = posicaoFinal estadoJogo,
      chavesCapturadas = c : chavesCapturadas estadoJogo
    }

{- Função auxiliar que executa o movimento correspondente a uma parede.
Pelo nosso entender do trabalho uma tentativa de movimentaçao necessita de uma nova instancia
do EstadoJogo, mesmo que permaneça tudo igual -}
movimentoParede :: EstadoJogo -> EstadoJogo
movimentoParede estadoJogo =
  EstadoJogo
    { lab = lab estadoJogo,
      posicao = posicao estadoJogo,
      posicaoFinal = posicaoFinal estadoJogo,
      chavesCapturadas = chavesCapturadas estadoJogo
    }

{- Função auxiliar que executa o movimento correspondente a um portal e que caso estejamos a tentar
mover para um deles iremos automcaticamente parar ao outro, atualizando assim o labirinto de acordo-}
movimentoPortal :: EstadoJogo -> (Int, Int) -> EstadoJogo
movimentoPortal estadoJogo pos =
  EstadoJogo
    { lab = verUltimoMov (lab estadoJogo) pos,
      posicao =
        if portais !! 0 == pos
          then portais !! 1
          else portais !! 0,
      posicaoFinal = posicaoFinal estadoJogo,
      chavesCapturadas = chavesCapturadas estadoJogo
    }
  where
    portais = findPortal (lab estadoJogo) 0

{- Função auxiliar que avalia o ultimo movimento e altera o labirinto consoante
o movimento colocando sempre o caracter correspondente na posição correta.
No caso de haver apenas 1 portal no ultimo movimento realizado significa que estamos num deles, ou seja,
vamos coloca um portal onde estamos e executamos o proximo movimento com o labirinto atualizado.
Seguimos esta logica com a posicao inicial, porque caso nao exista no labirinto significa que estamos nessa posição -}
verUltimoMov :: [String] -> (Int, Int) -> [String]
verUltimoMov labirinto pos
  | snd (posicaoCaracter labirinto 'S') == (-1) = colocaChar (colocaChar labirinto posP 'S') pos 'P' -- Verifica se estamos na posicao inicial
  | length (findPortal labirinto 0) == 1 = colocaChar (colocaChar labirinto posP '@') pos 'P' -- Verifica se estamos num portal
  | pos `elem` portais = colocaChar (colocaChar labirinto posP ' ') posPortal 'P' -- Verifica se o movimento que queremos executar é para um portal
  | otherwise = colocaChar (colocaChar labirinto posP ' ') pos 'P' -- Coloca apenas um espaco na posicao onde estavamos caso nao seja nenhum dos casos anteriores
  where
    posP = posicaoCaracter labirinto 'P'
    posPortal = if head portais == pos then portais !! 1 else head portais
    portais = findPortal labirinto 0

{- Função auxiliar que recebe um labirinto e
devolva as coordenadas de um portal (caracter '@') nesse labirinto -}
findPortal :: [String] -> Int -> [(Int, Int)]
findPortal [] _ = []
findPortal (x : xs) linha
  | '@' `elem` x = (findPortalAux x linha 0) ++ (findPortal xs (linha + 1))
  | otherwise = (findPortal xs (linha + 1))

{- Função auxiliar da função findPortal -}
findPortalAux :: String -> Int -> Int -> [(Int, Int)]
findPortalAux [] _ _ = []
findPortalAux (x : xs) linha coluna
  | x == '@' = (linha, coluna) : findPortalAux xs linha (coluna + 1)
  | otherwise = findPortalAux xs linha (coluna + 1)