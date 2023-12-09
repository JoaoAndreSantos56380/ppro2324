-- fc56326,fc56380~
-- se compilar o blackjack na "mesma" pasta do ghci, o import nao e feito corretamente
module Blackjack (resetGame, desconverte, applyHit, applyBet, GameState (..), Carta (..), Naipe (..), Valor (..), houseTurn, Estrategia, EstadoJogo (..), converte, tamanho, inicializa, creditos, Baralho, baralho, playRound, terminado, sempreStand, sempreHit, handValue, convenientHandValue, blackjacktstrat) where

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

desconverte :: Baralho -> [String]
desconverte = map cardToString
  where
    cardToString (Carta r s) = valorToChar r : [naipeToChar s]

    valorToChar Dois = '2'
    valorToChar Tres = '3'
    valorToChar Quatro = '4'
    valorToChar Cinco = '5'
    valorToChar Seis = '6'
    valorToChar Sete = '7'
    valorToChar Oito = '8'
    valorToChar Nove = '9'
    valorToChar Dez = 'T'
    valorToChar Valete = 'J'
    valorToChar Dama = 'Q'
    valorToChar Rei = 'K'
    valorToChar As = 'A'

    naipeToChar Copas = 'H'
    naipeToChar Paus = 'C'
    naipeToChar Ouros = 'D'
    naipeToChar Espadas = 'S'

tamanho :: Baralho -> Int
tamanho = length

data GameState = Initial | AskBet | AskHit | AfterHit | HouseTurn | Won | Lost | Tied | Terminated deriving (Eq, Enum)

data EstadoJogo = EstadoJogo
  { playerCredits :: Int, -- Current player credits
    currentBet :: Int, -- Current bet amount
    deck :: Baralho, -- Remaining deck of cards
    playerHand :: Baralho, -- Player's current hand
    dealerHand :: Baralho, -- Dealer's current hand
    state :: GameState -- What happened on the last state (useful to print messages)
  }
  deriving (Eq)

instance Show EstadoJogo where
  show EstadoJogo {playerCredits, currentBet, playerHand, dealerHand, deck} =
    "\nPlayer credits: "
      ++ show playerCredits
      ++ "\nCurrent Bet: "
      ++ show currentBet
      ++ "\nPlayer Hand ("
      ++ show (handValue playerHand)
      ++ " "
      ++ show (convenientHandValue playerHand)
      ++ "): "
      ++ show playerHand
      ++ "\nDealer Hand ("
      ++ show (handValue dealerHand)
      ++ " "
      ++ show (convenientHandValue dealerHand)
      ++ "): "
      ++ show dealerHand
      ++ "\nDeck size: "
      ++ show (tamanho deck)
      ++ " "
      ++ show (head deck)

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
handValue cards = map sum . sequence $ map cardValue cards

convenientHandValue :: [Carta] -> Int
convenientHandValue cards = if length (filter (< 22) (handValue cards)) > 1 then maximum (filter (< 22) (handValue cards)) else head (handValue cards)

inicializa :: Baralho -> EstadoJogo
inicializa deck = EstadoJogo 100 0 deck [] [] Initial

creditos :: EstadoJogo -> Int
creditos EstadoJogo {playerCredits} = playerCredits

baralho :: EstadoJogo -> Baralho
baralho EstadoJogo {deck} = deck

terminado :: EstadoJogo -> Bool
terminado EstadoJogo {playerCredits, currentBet, deck} = currentBet == 0 && (playerCredits <= 0 || length deck <= 20)

type Estrategia = Int -> Int -> Baralho -> Baralho -> (Int, Bool)

sempreStand :: Estrategia
sempreStand _ _ _ _ = (5, False)

sempreHit :: Estrategia
sempreHit _ _ _ _ = (5, True)

blackjacktstrat :: Estrategia
blackjacktstrat playerCredits _ _ _ = (5, playerCredits > 1)

houseTurn :: EstadoJogo -> EstadoJogo
houseTurn game@(EstadoJogo {deck, dealerHand}) =
  if convenientHandValue dealerHand < 17
    then houseTurn game {deck = tail deck, dealerHand = dealerHand++[head deck]}
    else game

playRound :: EstadoJogo -> Int -> Bool -> EstadoJogo
playRound game@EstadoJogo {playerHand, currentBet, playerCredits, dealerHand, deck, state} bet hit
  | state == HouseTurn && convenientHandValue playerHand > convenientHandValue dealerHand = game {{- playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = [], -} state = Won}
  | state == HouseTurn && convenientHandValue playerHand == convenientHandValue dealerHand = game {{- playerCredits = playerCredits + currentBet, currentBet = 0, playerHand = [], dealerHand = [], -} state = Tied}
  | state == HouseTurn && convenientHandValue dealerHand > 21 = game {{- playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = [], -} state = Won}
  | state == HouseTurn = game {{- currentBet = 0, playerHand = [], dealerHand = [], -} state = Lost}
  | convenientHandValue playerHand > 21 = game {{- currentBet = 0, playerHand = [], dealerHand = [], -} state = Lost}
  | convenientHandValue playerHand == 21 = playRound (houseTurn game{state = HouseTurn}) bet hit
  | otherwise = playRound (houseTurn game {state = HouseTurn}) bet hit

applyBet :: EstadoJogo -> Int -> EstadoJogo
applyBet game@EstadoJogo {playerCredits, deck} bet =
  let newGame =
        game
          { playerCredits = playerCredits - bet,
            currentBet = bet,
            deck = drop 4 deck,
            playerHand = take 2 deck,
            dealerHand = take 2 (drop 2 deck),
            state = AskHit
          }
   in newGame

applyHit :: EstadoJogo -> EstadoJogo
applyHit game@EstadoJogo {playerHand, deck} = game {playerHand = playerHand++[head deck], deck = tail deck}

resetGame:: EstadoJogo -> EstadoJogo
resetGame game@EstadoJogo {playerHand, currentBet, playerCredits, dealerHand, deck, state}
  | state == Won = game {playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = [], state = Won}
  | state == Tied = game {playerCredits = playerCredits + currentBet, currentBet = 0, playerHand = [], dealerHand = [], state = Tied}
  | otherwise = game {currentBet = 0, playerHand = [], dealerHand = [], state = Lost}
