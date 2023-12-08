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
-- mandar mail sobre isto!!!!!!!!!!!!!!!!!!!!

import Data.List
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO
import System.Random (mkStdGen, newStdGen)
import System.Random.Shuffle (shuffle, shuffle')

-- import Test.QuickCheck

exitIfQuit :: String -> IO ()
exitIfQuit "sair" = exitSuccess -- error "Program terminated by user."
exitIfQuit _ = return ()

playGameWithFile :: String -> IO ()
playGameWithFile file = do
  cardsArray <- ler file
  result <- jogaBlackjack (inicializa (converte cardsArray))
  putStrLn $ "final Credits: " ++ show result

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-t"] -> putStrLn "Option -t selected."
    ["-n", n] -> do
      exitIfQuit n
      cardsArray <- generateDeck (read n :: Int)
      result <- jogaBlackjack (inicializa (converte cardsArray))
      putStrLn $ "final Credits: " ++ show result
    [arg] -> do
      exitIfQuit arg
      playGameWithFile arg
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
  -- print shuffledDeck
  return shuffledDeck

jogaBlackjack :: EstadoJogo -> IO Int
jogaBlackjack game@EstadoJogo {playerHand, playerCredits, currentBet, state} = do
  if terminado game
    then
      if state == Won
        then do
          print "Vitoria"
          return (playerCredits + currentBet)
        else
          if state == Tied
            then do
              print "Empate"
              return (playerCredits + currentBet)
            else
              if state == Lost
                then do
                  print "Derrota"
                  return (playerCredits + currentBet)
                else return (playerCredits + currentBet)
    else
      if state == Initial
        then do
          print game
          if convenientHandValue playerHand == 21 then jogaBlackjack (houseTurn game {state = HouseTurn}) else jogaBlackjack game {state = AskBet}
        else
          if state == AskBet
            then do
              bet <- askBet
              if bet == -1
                then return (playerCredits + currentBet)
                else do
                  let newGameState =
                        game
                          { playerCredits = playerCredits - bet,
                            currentBet = bet,
                            deck = drop 4 (deck game),
                            playerHand = take 2 (deck game),
                            dealerHand = take 2 (drop 2 (deck game)),
                            state = AskHit
                          }
                  print newGameState
                  jogaBlackjack newGameState
            else
              if state == AskHit
                then
                  if convenientHandValue playerHand < 21
                    then do
                      hit <- askHit game
                      if hit
                        then do
                          let newGameState = game {playerHand = head (deck game) : playerHand, deck = tail (deck game)}
                          print newGameState
                          jogaBlackjack newGameState
                        else jogaBlackjack (playRound game currentBet False)
                    else jogaBlackjack (playRound game currentBet False)
                else
                  if state == Won
                    then do
                      print "Vitoria"
                      jogaBlackjack game {state = Initial}
                    else
                      if state == Tied
                        then do
                          print "Empate"
                          jogaBlackjack game {state = Initial}
                        else do
                          print "Derrota"
                          jogaBlackjack game {state = Initial}

askBet :: IO Int
askBet = do
  putStr "Enter your bet (format 'apostar n'): "
  hFlush stdout
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
  putStr "Enter your move (hit or stand): "
  hFlush stdout
  move <- getLine
  return (move == "hit")

{- prop_initialHandValue :: [Carta] -> Bool
prop_initialHandValue hand = length hand == 2 && convenientHandValue hand <= 21 -}

{- prop_playerCreditsAfterRound :: Int -> Bool -> Bool
prop_playerCreditsAfterRound bet hit =
  let state = EstadoJogo {playerCredits = 100 - bet, currentBet = bet, deck = [], playerHand = [], dealerHand = []}
      updatedState = playRound state bet hit
      expectedCredits =
        if convenientHandValue (playerHand updatedState) == 21
          then 100 + bet
          else
            if convenientHandValue (dealerHand updatedState) == 21
              then 100 - bet
              else 100
   in playerCredits updatedState == expectedCredits -}

{- prop_dealerHandValue :: [Carta] -> Bool
prop_dealerHandValue dealerHand =
  let finalValue = undefined -- Função que simula a jogada da casa e retorna o valor da mão
   in finalValue >= 17 -}

{- prop_deckIntegrity :: Baralho -> Bool
prop_deckIntegrity deck = length deck == 52 && length (nub deck) == length deck -}

{- prop_shuffleDeckIntegrity :: Baralho -> Bool
prop_shuffleDeckIntegrity deck =
  let shuffledDeck = shuffle deck
  in sort shuffledDeck == sort deck -}

{- prop_shuffleDeckIntegrity :: Baralho -> Bool
prop_shuffleDeckIntegrity deck =
  let seed = 52 -- or any other fixed number for reproducibility
      shuffledDeck = shuffle' deck (length deck) (mkStdGen seed)
   in sort shuffledDeck == sort deck -}

{- prop_betValue :: Int -> Int -> Bool
prop_betValue credits bet = bet >= 0 && bet <= credits -}
