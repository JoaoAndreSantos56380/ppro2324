-- fc56326,fc56380~
-- se compilar o blackjack na "mesma" pasta do ghci, o import nao e feito corretamente
module Blackjack (houseTurn, Estrategia, EstadoJogo(..), converte, tamanho, inicializa, creditos, Baralho, baralho, terminado, sempreStand, sempreHit{- , simulaRonda -}{- , simulaJogo -}, handValue, convenientHandValue, blackjacktstrat) where

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

data EstadoJogo = EstadoJogo
  { playerCredits :: Int, -- Current player credits
    currentBet :: Int, -- Current bet amount
    deck :: Baralho, -- Remaining deck of cards
    playerHand :: Baralho, -- Player's current hand
    dealerHand :: Baralho -- Dealer's current hand
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
inicializa deck = EstadoJogo 100 0 deck [] []

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

{- simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda e state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand})
  | currentBet == 0 = state {playerCredits = playerCredits - 5, currentBet = 5, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck)}
  | convenientHandValue playerHand > 21 = state {currentBet = 0, playerHand = [], dealerHand = []}
  | convenientHandValue playerHand == 21 = houseTurn state
  | snd (e playerCredits currentBet playerHand dealerHand) = state {playerHand = nextCard : playerHand, deck = nextDeck}
  | otherwise = houseTurn state
  where
    nextCard = head deck
    nextDeck = tail deck -}

{- simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo e deck = simulaJogoAux e (inicializa deck) -}

{- simulaJogoAux :: Estrategia -> EstadoJogo -> Int
simulaJogoAux e state@EstadoJogo {playerCredits, currentBet} =
  if terminado state
    then playerCredits + currentBet
    else simulaJogoAux e (simulaRonda e state) -}

houseTurn :: EstadoJogo -> IO EstadoJogo
houseTurn state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand}) = 
  if convenientHandValue dealerHand >= 17 then
      if convenientHandValue playerHand > convenientHandValue dealerHand then do
        print "Vitoria"
        return state {playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = []}
      else if convenientHandValue playerHand == convenientHandValue dealerHand then do
          print "Empate"
          return state {playerCredits = playerCredits + currentBet, currentBet = 0, playerHand = [], dealerHand = []}
      else if convenientHandValue dealerHand > 21 then do
        print "Vitoria"
        return state {playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = []}
      else do
        print "Derrota"
        return state {currentBet = 0, playerHand = [], dealerHand = []}
  else do
    let newState = state {deck = tail deck, dealerHand = head deck : dealerHand}
    print newState
    houseTurn newState
