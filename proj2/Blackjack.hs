-- fc56326,fc56380
{- module Blackjack
  ( converte,
    tamanho {- , inicializa, creditos, baralho, terminado, sempreStand, sempreHit, simulaRonda, simulaJogo -},
  )
where -}

import BaralhosExemplo
import Graphics.Win32 (scrollBarStyle)

data Naipe = Copas | Ouros | Paus | Espadas deriving (Show, Eq)

data Valor = Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei | As deriving (Show, Eq)

data Carta = Carta {value :: Valor, suit :: Naipe} deriving (Show, Eq)

type Baralho = [Carta]

converte :: [String] -> Baralho
converte = map stringToCard
  where
    stringToCard [r, s] = Carta (charToValor r) (charToNaipe s)
    stringToCard _ = error "Cartada mal feita"

    charToValor '2' = Dois
    charToValor '3' = Tres
    charToValor '4' = Quatro
    charToValor '5' = Cinco
    charToValor '6' = Seis
    charToValor '7' = Sete
    charToValor '8' = Oito
    charToValor '9' = Nove
    charToValor 'T' = Dez
    charToValor 'J' = Valete
    charToValor 'Q' = Dama
    charToValor 'K' = Rei
    charToValor 'A' = As
    charToValor _ = error "Valor invalido"

    charToNaipe 'H' = Copas
    charToNaipe 'C' = Paus
    charToNaipe 'D' = Ouros
    charToNaipe 'S' = Espadas
    charToNaipe _ = error "Naipe invalido"

tamanho :: Baralho -> Int
tamanho = length

data CurrentPlayer = House | Player deriving (Show)

data EstadoJogo = EstadoJogo
  { playerCredits :: Int, -- Current player credits
    currentBet :: Int, -- Current bet amount
    deck :: Baralho, -- Remaining deck of cards
    playerHand :: Baralho, -- Player's current hand
    dealerHand :: Baralho, -- Dealer's current hand
    housePlay :: Bool -- Signals the end of a round
  }
  deriving (Eq)

instance Show EstadoJogo where
  show EstadoJogo {playerCredits, currentBet, playerHand, dealerHand, housePlay} =
    "Player credits: "
      ++ show playerCredits
      ++ "\nCurrent Bet: "
      ++ show currentBet
      ++ "\nPlayer Hand ("
      ++ show (handValue playerHand)
      ++ "): "
      ++ show playerHand
      ++ "\nDealer Hand ("
      ++ show (handValue dealerHand)
      ++ "): "
      ++ show dealerHand
      ++ "\nHouse Turn: "
      ++ show housePlay

cardValue :: Carta -> [Int]
cardValue Carta {value}
  | value == As = [1, 11]
  | value == Dois = [2]
  | value == Tres = [3]
  | value == Quatro = [4]
  | value == Cinco = [5]
  | value == Seis = [6]
  | value == Sete = [7]
  | value == Oito = [8]
  | value == Nove = [9]
  | value == Dez = [10]
  | value == Valete = [10]
  | value == Dama = [10]
  | value == Rei = [10]

handValue :: [Carta] -> [Int]
handValue cards = [left, right]
  where
    left = sum (map (head . cardValue) cards)
    right = sum (map (last . cardValue) cards)

-- The inicialization function
inicializa :: Baralho -> EstadoJogo
inicializa deck = EstadoJogo initPlayerCredits initCurrentBet initDeck initPlayerHand initDealerHand initHousePlay
  where
    initPlayerCredits = 100
    initCurrentBet = 5
    initDeck = drop 4 deck
    initPlayerHand = take 2 deck
    initDealerHand = take 2 (drop 2 deck)
    initHousePlay = False

creditos :: EstadoJogo -> Int
creditos EstadoJogo {playerCredits} = playerCredits

baralho :: EstadoJogo -> Baralho
baralho EstadoJogo {deck} = deck

terminado :: EstadoJogo -> Bool
terminado EstadoJogo {playerCredits, deck} = playerCredits <= 0 || length deck <= 20

type Estrategia = EstadoJogo -> Bool

sempreStand :: Estrategia
sempreStand _ = False

sempreHit :: Estrategia
sempreHit _ = True

blackjacktstrat :: Estrategia
blackjacktstrat EstadoJogo {playerCredits} = playerCredits > 1

simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda e EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand, housePlay} = ejNext
  where
    ejNext =
      if e EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand, housePlay}
        then EstadoJogo {playerCredits = playerCredits, currentBet = currentBet, playerHand = nextCard : playerHand, deck = nextDeck, dealerHand = dealerHand, housePlay = nextHousePlay}
        else EstadoJogo {playerCredits = playerCredits, currentBet = currentBet, deck = nextDeck, playerHand = playerHand, dealerHand = dealerHand, housePlay = True}
    nextCard = head deck
    nextDeck = tail deck
    nextHousePlay = maximum (handValue (nextCard : playerHand)) == 21

simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo e deck = simulaJogoAux e (inicializa deck)

simulaJogoAux :: Estrategia -> EstadoJogo -> Int
simulaJogoAux e state = if terminado state then houseTurn state else simulaJogoAux e nextState
  where
    score = (\EstadoJogo {playerCredits} -> playerCredits) state
    nextState = simulaRonda e state

houseTurn :: EstadoJogo -> Int
houseTurn EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand, housePlay} =
  if dealerScore >= 17
    then
      ( if dealerScore > 21 || playerScore > dealerScore
          then playerCredits + currentBet * 2
          else
            if playerScore == dealerScore
              then playerCredits
              else playerCredits - currentBet
      )
    else houseTurn EstadoJogo {playerCredits = playerCredits, currentBet = currentBet, deck = nextDeck, playerHand = playerHand, dealerHand = nextCard : dealerHand, housePlay = housePlay}
  where
    nextCard = head deck
    nextDeck = tail deck
    dealerScore = max (head (handValue dealerHand)) (last (handValue dealerHand))
    playerScore = max (head (handValue playerHand)) (last (handValue playerHand))

-- dealerScore = min (head (handValue dealerHand)) (last (handValue dealerHand))
-- playerScore = min (head (handValue playerHand)) (last (handValue playerHand))
-- dealerScore = head (handValue dealerHand)
-- playerScore = head (handValue playerHand)
-- dealerScore = last (handValue dealerHand)
-- playerScore = last (handValue playerHand)
-- dealerScore = head (handValue dealerHand)
-- playerScore = last (handValue playerHand)
-- dealerScore = last (handValue dealerHand)
-- playerScore = head (handValue playerHand)

main :: IO ()
main = do
  let ejOrdenado = inicializa (converte baralhoOrdenado)
  -- print (houseTurn game)
  -- print (simulaJogoAux sempreStand game)
  print (simulaJogo sempreStand (converte baralhoOrdenado))
  print (tamanho (baralho ejOrdenado))
  let nextState0 = simulaRonda sempreHit ejOrdenado
  putStrLn (show ejOrdenado ++ "\n")
  putStrLn (show nextState0 ++ "\n")

-- let nextState1 = simulaRonda sempreStand nextState0
-- putStrLn (show nextState1 ++ "\n")
{- let nextState2 = simulaRonda sempreStand nextState1
putStrLn (show nextState2 ++ "\n")
let nextState3 = simulaRonda sempreStand nextState2
putStrLn (show nextState3 ++ "\n")
let nextState4 = simulaRonda sempreStand nextState3
putStrLn (show nextState4 ++ "\n") -}

-- let ej1 = simulaRonda sempreStand ejOrdenado
-- print (tamanho (baralho ej1))

-- print (simulaJogo sempreHit (converte baralhoOrdenado))
