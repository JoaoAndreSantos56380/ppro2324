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
import Data.List
-- Import your tests

import Data.Time.Format.ISO8601 (hourFormat)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO
import System.Random (mkStdGen, newStdGen)
import System.Random.Shuffle (shuffle, shuffle')
import Test.QuickCheck
import Testes

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-t"] -> do
      putStrLn "Running tests..."
      quickCheck prop_initialHandValue
      quickCheck prop_CreditsAfterRound
      quickCheck prop_houseTurnValue
      quickCheck prop_gameAfterRound
      quickCheck prop_initialPlayerHandSize
      quickCheck prop_initialDealerHandSize
      quickCheck prop_playerHandSizeAfterHit
      quickCheck prop_cardDistribution
      quickCheck prop_endRound
    ["-n", n] -> do
      cardsArray <- generateDeck (read n :: Int)
      result <- jogaBlackjack (inicializa (converte cardsArray))
      putStrLn $ "saldo final: " ++ show (playerCredits result)
    [arg] -> do
      if arg == "sair"
        then exitSuccess
        else do
          contents <- readFile arg
          let cardsArray = lines contents
          result <- jogaBlackjack (inicializa (converte cardsArray))
          putStrLn $ "saldo final: " ++ show (playerCredits result)
    [] -> do
      contents <- readFile "default.bar"
      let cardsArray = lines contents
      result <- jogaBlackjack (inicializa (converte cardsArray))
      putStrLn $ "saldo final: " ++ show (playerCredits result)
    _ -> putStrLn "usage: ./Main <input_file>"

generateDeck :: Int -> IO [String]
generateDeck n = do
  rng <- newStdGen
  let deck = [[valor, naipe] | naipe <- "SHDC", valor <- "A23456789TJQK", _ <- [1 .. n]]
  let shuffledDeck = shuffle' deck (length deck) rng
  -- print shuffledDeck
  return shuffledDeck

{- startGame :: EstadoJogo -> IO Int
startGame game = do
  printCardsAndCredits game
  finalGameState <- jogaBlackjack game
  printCardsAndCredits finalGameState
  return (playerCredits finalGameState + currentBet finalGameState) -}

jogaBlackjack :: EstadoJogo -> IO EstadoJogo
jogaBlackjack game@EstadoJogo {playerHand, playerCredits, currentBet, state} = do
  if terminado game
    then
      if state == Won
        then do
          putStrLn "Vitoria"
          printCardsAndCredits game
          return game
        else
          if state == Tied
            then do
              putStrLn "Empate"
              printCardsAndCredits game
              return game
            else
              if state == Lost
                then do
                  putStrLn "Derrota"
                  printCardsAndCredits game
                  return game
                else return game -- sair
    else
      if state == Initial
        then do
          -- print game
          if convenientHandValue playerHand == 21
            then do
              let gameAfterHouseTurn = houseTurn game {state = HouseTurn}
              printPlayerAndDealerDecks gameAfterHouseTurn
              jogaBlackjack gameAfterHouseTurn
            else jogaBlackjack game {state = AskBet}
        else
          if state == AskBet
            then do
              printCardsAndCredits game
              bet <- askBet
              if bet == -1
                then return game
                else do
                  let newGameState = applyBet game bet
                  printPlayerAndDealerDecks newGameState
                  if convenientHandValue (getPlayerHand newGameState) == 21
                    then do
                      let gameAfterHouseTurn = houseTurn newGameState {state = HouseTurn}
                      printPlayerAndDealerDecks gameAfterHouseTurn
                      jogaBlackjack gameAfterHouseTurn {state = AskHit}
                    else do
                      jogaBlackjack newGameState
            else
              if state == AskHit
                then
                  if convenientHandValue playerHand < 21
                    then do
                      hit <- askHit game
                      if hit
                        then do
                          let newGameState = applyHit game
                          -- print newGameState
                          printPlayerAndDealerDecks newGameState
                          if convenientHandValue (getPlayerHand newGameState) == 21 then do
                            let gameAfterHouseTurn = houseTurn newGameState
                            printPlayerAndDealerDecks gameAfterHouseTurn
                            let reset = resetGame (playRound gameAfterHouseTurn currentBet False)
                            jogaBlackjack reset
                          else jogaBlackjack newGameState
                        else do
                          let gameAfterPlayRound = playRound game currentBet False
                          printPlayerAndDealerDecks gameAfterPlayRound
                          let reset = resetGame gameAfterPlayRound
                          jogaBlackjack reset -- gameAfterPlayRound
                    else do
                      -- let gameAfterPlayRound = playRound game currentBet False
                      let gameAfterHouseTurn = houseTurn game
                      --printPlayerAndDealerDecks gameAfterHouseTurn
                      --printPlayerAndDealerDecks gameAfterPlayRound
                      let reset = resetGame (playRound gameAfterHouseTurn currentBet False)
                      jogaBlackjack reset
                else
                  if state == Won
                    then do
                      putStrLn "Vitoria"
                      jogaBlackjack game {state = Initial}
                    else
                      if state == Tied
                        then do
                          putStrLn "Empate"
                          jogaBlackjack game {state = Initial}
                        else do
                          putStrLn "Derrota"
                          jogaBlackjack game {state = Initial}

getPlayerHand :: EstadoJogo -> Baralho
getPlayerHand = playerHand

askBet :: IO Int
askBet = do
  -- putStr "Enter your bet (format 'apostar n'): "
  -- hFlush stdout
  input <- getLine
  let inputWords = words input
  if head inputWords == "sair"
    then return (-1)
    else
      if length inputWords == 2 && head inputWords == "apostar"
        then return (read (inputWords !! 1) :: Int)
        else do
          putStrLn "Invalid input format. Please enter in 'apostar n' format."
          askBet

askHit :: EstadoJogo -> IO Bool
askHit game@EstadoJogo {playerCredits, currentBet} = do
  -- putStr "Enter your move (hit or stand): "
  -- hFlush stdout
  move <- getLine
  return (move == "hit")

printCardsAndCredits :: EstadoJogo -> IO ()
printCardsAndCredits game = do
  putStrLn $ "cartas: " ++ show (length (deck game))
  putStrLn $ "creditos: " ++ show (playerCredits game)

{- printPlayerAndDealerDecks :: EstadoJogo -> IO ()
printPlayerAndDealerDecks game = do
    putStrLn $ "jogador: " ++ show (desconverte (playerHand game))
    putStrLn $ "casa: " ++ show (desconverte (dealerHand game)) -}
printPlayerAndDealerDecks :: EstadoJogo -> IO ()
printPlayerAndDealerDecks game = do
  putStrLn $ "jogador: " ++ (unwords . desconverte $ playerHand game)
  putStrLn $ "casa: " ++ (unwords . desconverte $ dealerHand game)
