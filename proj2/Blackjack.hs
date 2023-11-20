-- fc56326,fc56380
{- module Blackjack
  ( converte,
    tamanho {- , inicializa, creditos, baralho, terminado, sempreStand, sempreHit, simulaRonda, simulaJogo -},
  )
where -}
-- num site, tive uma ronda em que ambos tiveram 21 pontos e eu perdi (tinha mais cartas que a casa)

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
  { status :: String,
    playerCredits :: Int, -- Current player credits
    currentBet :: Int, -- Current bet amount
    deck :: Baralho, -- Remaining deck of cards
    playerHand :: Baralho, -- Player's current hand
    dealerHand :: Baralho -- Dealer's current hand
  }
  deriving (Eq)

instance Show EstadoJogo where
  show EstadoJogo {playerCredits, currentBet, playerHand, dealerHand, deck, status} =
    "\n"
      ++ show status
      ++ "\nPlayer credits: "
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

-- The inicialization function
inicializa :: Baralho -> EstadoJogo
inicializa deck = EstadoJogo "STARTING GAME" 100 0 deck [] []

creditos :: EstadoJogo -> Int
creditos EstadoJogo {playerCredits} = playerCredits

baralho :: EstadoJogo -> Baralho
baralho EstadoJogo {deck} = deck

terminado :: EstadoJogo -> Bool
terminado EstadoJogo {playerCredits, currentBet, deck} = currentBet == 0 && (playerCredits <= 0 || length deck <= 20)

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

simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda e state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand})
  | currentBet == 0 = state {status = "#### (0) Nova ronda! ####", playerCredits = playerCredits - 5, currentBet = 5, deck = drop 4 deck, playerHand = take 2 deck, dealerHand = take 2 (drop 2 deck)}
  | convenientHandValue playerHand > 21 = state {status = "#### (1) PERDEU POERQUE TEM MAIS DE 21 PONTOS ####", currentBet = 0, playerHand = [], dealerHand = []}
  | convenientHandValue playerHand == 21 = houseTurn state {status = "### HOUSE TURN BECAUSE PLAYER HAS 21 POINTS ###"}
  | e state = state {status = "### HIT ###", playerHand = nextCard : playerHand, deck = nextDeck}
  | otherwise = houseTurn state {status = "### HOUSE TURN BECAUSE PLAYER STOOD ###"}
  where
    nextCard = head deck
    nextDeck = tail deck

simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo e deck = simulaJogoAux e (inicializa deck)

simulaJogoAux :: Estrategia -> EstadoJogo -> Int
simulaJogoAux e state@EstadoJogo {playerCredits, currentBet} =
  if terminado state
    then playerCredits + currentBet
    else simulaJogoAux e (simulaRonda e state)

houseTurn :: EstadoJogo -> EstadoJogo
houseTurn state@(EstadoJogo {playerCredits, currentBet, deck, playerHand, dealerHand}) =
  if convenientHandValue dealerHand >= 17
    then
      if convenientHandValue playerHand > convenientHandValue dealerHand
        then state {status = "#### (2) GANHOU PORQUE TEM MAIS PONTOS QUE O DEALER ####", playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = []}
        else
          if convenientHandValue playerHand == convenientHandValue dealerHand
            then state {status = "#### EMPATOU ####", playerCredits = playerCredits + currentBet, currentBet = 0, playerHand = [], dealerHand = []}
            else
              if convenientHandValue dealerHand > 21
                then state {status = "#### (3) GANHOU PORQUE O DEALER TEM MAIS DE 21 PONTOS ####", playerCredits = playerCredits + currentBet * 2, currentBet = 0, playerHand = [], dealerHand = []}
                else state {status = "#### (4) PERDEU PORQUE O DEALER TEM MAIS PONTOS ####", currentBet = 0, playerHand = [], dealerHand = []}
    else houseTurn state {status = "### DISTRIBUTE CARDS TO DEALER BECAUSE DEALER HAS <17 POINTS ###", deck = tail deck, dealerHand = head deck : dealerHand}

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
  let finalCredits = simulaJogo sempreStand (converte baralhoOrdenado)
  putStrLn $ "Final credits after playing with sempreStand strategy: 20 | " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoOrdenado)
  putStrLn $ "Final credits after playing with sempreHit strategy: 0 | " ++ show finalCredits

  let finalCredits = simulaJogo sempreStand (converte baralhoInsuficiente)
  putStrLn $ "Final credits after playing with sempreStand strategy: 100 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoInsuficiente)
  putStrLn $ "Final credits after playing with sempreHit strategy: 100 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreStand (converte baralhoSimples)
  putStrLn $ "Final credits after playing with sempreStand strategy: 105 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoSimples)
  putStrLn $ "Final credits after playing with sempreHit strategy: 95 | " ++ show finalCredits

  let finalCredits = simulaJogo sempreStand (converte baralhoEmpate)
  putStrLn $ "Final credits after playing with sempreStand strategy: 100 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoEmpate)
  putStrLn $ "Final credits after playing with sempreHit strategy: 100 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreStand (converte baralhoPerdido)
  putStrLn $ "Final credits after playing with sempreStand strategy: 95 | " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoPerdido)
  putStrLn $ "Final credits after playing with sempreHit strategy: 90 | " ++ show finalCredits

  let finalCredits = simulaJogo sempreStand (converte baralhoGanho)
  putStrLn $ "Final credits after playing with sempreStand strategy: 110 |  " ++ show finalCredits

  let finalCredits = simulaJogo sempreHit (converte baralhoGanho)
  putStrLn $ "Final credits after playing with sempreStand strategy: 105 |  " ++ show finalCredits

  -- Additional examples (commented out). Uncomment as needed.
  -- finalCredits2 <- simulaJogo sempreHit (converte baralhoOrdenado)
  -- putStrLn $ "Final credits after playing with sempreHit strategy: " ++ show finalCredits2

  -- finalCredits3 <- simulaJogo sempreStand (converte baralhoInsuficiente)
  -- putStrLn $ "Final credits after playing with sempreStand strategy and insufficient deck: " ++ show finalCredits3

  -- finalCredits4 <- simulaJogo sempreHit (converte baralhoInsuficiente)
  -- putStrLn $ "Final credits after playing with sempreHit strategy and insufficient deck: " ++ show finalCredits4

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
