import Blackjack
import Data.List
import Test.QuickCheck

-- Define Arbitrary instance for Carta if not already defined
instance Arbitrary Carta where
  arbitrary = do
    val <- elements [Dois, Tres, Quatro, Cinco, Seis, Sete, Oito, Nove, Dez, Valete, Dama, Rei, As]
    suit <- elements [Copas, Ouros, Paus, Espadas]
    return (Carta val suit)

-- Define Arbitrary instance for EstadoJogo
instance Arbitrary EstadoJogo where
  arbitrary = do
    -- Generate a deck of cards
    deck <- arbitrary `suchThat` (\deck -> length deck >= 20)
    -- Generate hands for the player and the house
    playerHand <- arbitrary `suchThat` (\hand -> length hand >= 2 && convenientHandValue hand <= 21)
    dealerHand <- arbitrary `suchThat` (\hand -> length hand >= 2 && convenientHandValue hand <= 21)
    -- Generate player credits
    playerCredits <- arbitrary `suchThat` (> 0)
    -- Construct an EstadoJogo
    return $ EstadoJogo {playerCredits = playerCredits, currentBet = 0, deck = deck, playerHand = playerHand, dealerHand = dealerHand, state = Initial}

-- Em dúvida
{- prop_initialHandValue :: [Carta] -> Property
prop_initialHandValue hand = length hand == 2 ==> convenientHandValue hand <= 21 -}

-- P1
prop_initialHandValue :: Carta -> Carta -> Bool
prop_initialHandValue left right = convenientHandValue [left, right] <= 21

-- P2
prop_CreditsAfterRound :: Int -> Int -> Bool -> EstadoJogo -> Property
prop_CreditsAfterRound n a standOrHit gameState =
  n > 0
    && a > 0
    && a
      <= n
        ==> let initialGameState =
                  applyBet gameState a
                newState = playRound initialGameState a standOrHit
                finalCredits = playerCredits newState
             in finalCredits == playerCredits newState - a || finalCredits == playerCredits newState || finalCredits == playerCredits newState + a

prop_gameAfterRound :: Int -> Bool -> EstadoJogo -> Property
prop_gameAfterRound bet hit game =
  bet > 0
    && bet
      <= playerCredits game
        ==> let finalGameState = playRound (applyBet game bet) bet hit
                finalPlayerHand = playerHand finalGameState
                finalDealerHand = dealerHand finalGameState
                finalDeck = deck finalGameState
             in null finalPlayerHand && null finalDealerHand && length finalDeck < length (deck game)

{-
game{state = Initial} X X => playerCredits = +aposta | -aposta | =, playerHand = [], dealerHand = [], length deck < length deck inicial
-}

-- P3
prop_houseTurnValue :: EstadoJogo -> Bool
prop_houseTurnValue game = convenientHandValue (dealerHand (houseTurn game)) >= 17

-- P4 (Depois de uma aposta, temos 2 cartas no player)
prop_deckIntegrity :: Baralho -> Property
prop_deckIntegrity deck = length deck == 52 ==> length (nub deck) == length deck

-- P5 (Depois de uma aposta, temos 2 cartas no dealer)

-- P6 (Depois de um hit, o player tem mais um carta)

-- P7 (Ao iniciar uma ronda, o player tem as 2 primeiras cartas e o dealer tem as 2 segundos cartas)

-- P8 (No final de uma ronda, as mãos de player e dealer+baralho atual = baralho original)

{- prop_shuffleDeckIntegrity :: Baralho -> Bool
prop_shuffleDeckIntegrity deck =
  let shuffledDeck = shuffle deck
  in sort shuffledDeck == sort deck -}

{- prop_shuffleDeckIntegrity :: Baralho -> Bool
prop_shuffleDeckIntegrity deck =
  let seed = 52 -- or any other fixed number for reproducibility
      shuffledDeck = shuffle' deck (length deck) (mkStdGen seed)
   in sort shuffledDeck == sort deck -}

prop_betValue :: Int -> Int -> Bool
prop_betValue credits bet = bet >= 0 && bet <= credits
