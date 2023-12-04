{-
Princípios de Programação
Projeto 3 - Modelo de submissão

\* A vossa submissão deverá ser composta por um único ficheiro zip
p3_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
\* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
\* O vosso código deverá ser compilável com uma instrução do tipo

> stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes quatro tipos de instruções:

> ./Main ficheiro -- carrega um baralho para jogar Blackjack
> ./Main          -- carrega o baralho default.bar
> ./Main -n X     -- carrega um baralho aleatório formado por X baralhos normais de cartas
> ./Main -t       -- corre os testes
-}

import Blackjack
import System.Environment (getArgs) -- mandar mail sobre isto!!!!!!!!!!!!!!!!!!!!
import System.IO
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-t"] -> putStrLn "Option -t selected."
    ["-n", n] -> do
      cardsArray <- generateDeck (read n::Int)
      result <- jogaBlackjack (inicializa (converte cardsArray))
      putStrLn $ "final Credits: " ++ show result
    [arg] -> putStrLn $ arg ++ " selected."
    [] -> do
      cardsArray <- ler "default.bar"
      result <- jogaBlackjack (inicializa (converte cardsArray))
      putStrLn $ "final Credits: " ++ show result
    _ -> putStrLn "usage: ./Main <input_file>"

ler :: String -> IO [String]
ler file = do
  contents <- readFile file
  return (lines contents)

generateDeck :: Int -> IO [String]
generateDeck n = do
  rng <- newStdGen
  let deck = [[valor, naipe] | naipe <- "SHDC", valor <- "A23456789TJQK", _ <- [1 .. n]]
  let shuffledDeck = shuffle' deck (length deck) rng
  --print shuffledDeck
  return shuffledDeck


-- ligar o meu chat gpt ao cursor!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
jogaBlackjack :: EstadoJogo -> IO Int
jogaBlackjack state = do
  if terminado state
    then return (playerCredits state + currentBet state)
    else do
      newState@EstadoJogo {playerCredits, currentBet, playerHand, deck} <- if currentBet state == 0 then askBet state else return state
      if convenientHandValue playerHand > 21
        then do
          print "Derrota"
          jogaBlackjack newState {currentBet = 0, playerHand = [], dealerHand = []}
        else
          if convenientHandValue playerHand == 21
            then do
              houseTurnState <- houseTurn newState
              jogaBlackjack houseTurnState
            else do
              hit <- askMove newState
              if hit
                then jogaBlackjack newState {playerHand = head deck : playerHand, deck = tail deck}
                else do
                  houseTurnState <- houseTurn newState
                  jogaBlackjack houseTurnState

{- jogaBlackjack :: EstadoJogo -> Int
jogaBlackjack state@EstadoJogo{playerCredits, currentBet}
  | currentBet == 0 = jogaBlackjack (askBet state)
  | terminado state = playerCredits + currentBet
  | otherwise = jogaBlackjack (simulateRound (askMove state) state) -}

-- ler aposta da consola e criar estado novo
askBet :: EstadoJogo -> IO EstadoJogo
askBet state@EstadoJogo {playerCredits, currentBet, deck} = do
  print state
  putStr "Enter your bet: "
  hFlush stdout
  bet <- getLine
  return state {playerCredits = playerCredits - read bet :: Int, currentBet = read bet :: Int, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck)}

{- askBet :: EstadoJogo -> EstadoJogo
askBet state@EstadoJogo{playerCredits, currentBet} = state -}

-- imprimir estado e pedir move ao user na consola
askMove :: EstadoJogo -> IO Bool
askMove state@EstadoJogo {playerCredits, currentBet} = do
  print state
  putStr "Enter your move (Hit or Stand): "
  hFlush stdout
  move <- getLine
  return (move == "Hit")

{- askMove :: EstadoJogo -> Bool
askMove state@EstadoJogo{playerCredits, currentBet} = False -}
