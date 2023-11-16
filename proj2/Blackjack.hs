-- fc56326,fc56380
{- module Blackjack
  ( converte,
    tamanho {- , inicializa, creditos, baralho, terminado, sempreStand, sempreHit, simulaRonda, simulaJogo -},
  )
where -}

import BaralhosExemplo

-- import Graphics.Win32 (scrollBarStyle)

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
    end :: String
  }
  deriving (Eq)

instance Show EstadoJogo where
  show EstadoJogo {playerCredits, currentBet, playerHand, dealerHand, deck, end} =
    "Player credits: "
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
      ++ "\n"
      ++ show end

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

-- The inicialization function
inicializa :: Baralho -> EstadoJogo
inicializa deck = EstadoJogo initPlayerCredits initCurrentBet initDeck initPlayerHand initDealerHand initEnd
  where
    initPlayerCredits = 100
    initCurrentBet = 5
    initDeck = drop 4 deck
    initPlayerHand = take 2 deck
    initDealerHand = take 2 (drop 2 deck)
    initEnd = ""

creditos :: EstadoJogo -> Int
creditos EstadoJogo {playerCredits} = playerCredits

baralho :: EstadoJogo -> Baralho
baralho EstadoJogo {deck} = deck

terminado :: EstadoJogo -> Bool
terminado EstadoJogo {playerCredits, deck} = playerCredits <= 0 || length deck < 20

type Estrategia = EstadoJogo -> Bool

sempreStand :: Estrategia
sempreStand _ = False

sempreHit :: Estrategia
sempreHit _ = True

blackjacktstrat :: Estrategia
blackjacktstrat EstadoJogo {playerCredits} = playerCredits > 1

{-
convenientValue
  | maximum hand > 21 = minimum hand
  | head hand == 21 || last hand == 21 = 21
  | otherwise = maximum hand
-}

simulaRonda :: Estrategia -> EstadoJogo -> IO EstadoJogo
simulaRonda e state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand}) = do
  if convenientHandValue playerHand > 21
    then return $ state {playerCredits = playerCredits - currentBet, playerHand = take 2 nextDeck, deck = drop 4 nextDeck, dealerHand = take 2 (drop 2 nextDeck), end = "#### (1) PERDEU POERQUE TEM MAIS DE 21 PONTOS ####"}
    else
      if convenientHandValue playerHand == 21
        then houseTurn state {end = ""}
        else
          if e state
            then return $ state {playerHand = nextCard : playerHand, deck = nextDeck, end = ""}
            else houseTurn state {end = ""}
  where
    nextCard = head deck
    nextDeck = tail deck

simulaJogo :: Estrategia -> Baralho -> IO Int
simulaJogo e deck = simulaJogoAux e (inicializa deck)

simulaJogoAux :: Estrategia -> EstadoJogo -> IO Int
simulaJogoAux e state@EstadoJogo {playerCredits} = do
  putStrLn (show state ++ "\n")
  if terminado state
    then return playerCredits
    else do
      newState <- simulaRonda e state
      simulaJogoAux e newState

houseTurn :: EstadoJogo -> IO EstadoJogo
houseTurn state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand}) = do
  putStrLn (show state ++ "\n")
  if convenientHandValue dealerHand >= 17
    then
      return $
        if convenientHandValue playerHand > convenientHandValue dealerHand
          then state {playerCredits = playerCredits + currentBet * 2, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck), end = "#### (2) GANHOU PORQUE TEM MAIS PONTOS QUE O DEALER ####"}
          else
            if convenientHandValue playerHand == convenientHandValue dealerHand
              then state {playerCredits = playerCredits, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck), end = "#### EMPATOU ####"}
              else
                if convenientHandValue dealerHand > 21
                  then state {playerCredits = playerCredits + currentBet * 2, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck), end = "#### (3) GANHOU PORQUE O DEALER TEM MAIS DE 21 PONTOS ####"}
                  else state {playerCredits = playerCredits - currentBet, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck), end = "#### (4) PERDEU PORQUE O DEALER TEM MAIS PONTOS ####"}
    else houseTurn state {deck = tail deck, dealerHand = head deck : dealerHand}

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
  -- Uncomment any of the following lines to run simulations with different strategies and decks.

  -- Simulate the game with a "stand" strategy and a simple deck.
  finalCredits <- simulaJogo sempreStand (converte baralhoSimples)
  putStrLn $ "Final credits after playing with sempreStand strategy: " ++ show finalCredits

  -- Additional examples (commented out). Uncomment as needed.
  -- finalCredits2 <- simulaJogo sempreHit (converte baralhoOrdenado)
  -- putStrLn $ "Final credits after playing with sempreHit strategy: " ++ show finalCredits2

  -- finalCredits3 <- simulaJogo sempreStand (converte baralhoInsuficiente)
  -- putStrLn $ "Final credits after playing with sempreStand strategy and insufficient deck: " ++ show finalCredits3

  -- finalCredits4 <- simulaJogo sempreHit (converte baralhoInsuficiente)
  -- putStrLn $ "Final credits after playing with sempreHit strategy and insufficient deck: " ++ show finalCredits4

  putStrLn "Cool beans, man!"


{-
Jogo começa com a distribuição de 2 cartas para o jogador e 2 cartas para o dealer. ✅
Se o valor mais conveniente da mão do jogador > 21, então jogador perde ✅
Se o valor mais conveniente da mão do jogador == 21, então inicia "house play" ✅
Se o valor mais conveniente da mão do jogador < 21, então jogador decide stand ou hit ✅
  Se hit, então decide outra vez ✅
  Se stand, então iniciamos "house play" ✅
House Play ✅
  Se convenientHandValue dealerHand < 17, então distribuir carta para o dealer ✅
  Se convenientHandValue dealerHand >= 17, então calcula pontos ✅
    Se convenientHandValue playerHand > convenientHandValue dealerHand, então jogador ganha ✅
    Se convenientHandValue playerHand == convenientHandValue dealerHand, então empatam ✅
    Se convenientHandValue playerHand < convenientHandValue dealerHand, então ✅
      Se convenientHandValue dealerHand > 21, então jogador ganha ✅
      Se convenientHandValue dealerHand <= 21, então jogador perde ✅

-}
