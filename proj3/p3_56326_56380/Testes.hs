module Testes (prop_initialHandValue, prop_CreditsAfterRound, prop_gameAfterRound, prop_houseTurnValue, prop_playerHandSizeAfterHit, prop_cardDistribution, Carta, EstadoJogo) where

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

-- P1
prop_initialHandValue :: Carta -> Carta -> Bool
prop_initialHandValue left right = convenientHandValue [left, right] <= 21

-- P2
prop_CreditsAfterRound :: Int -> Int -> Bool -> EstadoJogo -> Property
prop_CreditsAfterRound n a standOrHit gameState =
  n > 0 && a > 0 && a <= n
        ==> let initialGameState =
                  applyBet gameState a
                newState = playRound initialGameState a standOrHit
                finalCredits = playerCredits newState
             in finalCredits == playerCredits newState - a || finalCredits == playerCredits newState || finalCredits == playerCredits newState + a

-- P3
prop_houseTurnValue :: EstadoJogo -> Bool
prop_houseTurnValue game = convenientHandValue (dealerHand (houseTurn game)) >= 17

-- P4 (baralho tem sempre menos cartas no final de uma ronda)
prop_gameAfterRound :: Int -> Bool -> EstadoJogo -> Property
prop_gameAfterRound bet hit game =
  bet > 0
    && bet
      <= playerCredits game
        ==> let finalGameState = playRound (applyBet game bet) bet hit
                finalPlayerHand = playerHand finalGameState
                finalDealerHand = dealerHand finalGameState
                finalDeck = deck finalGameState
             in length finalDeck < length (deck game)

-- P5 (Depois de um hit, o player tem mais um carta)
prop_playerHandSizeAfterHit :: EstadoJogo -> Bool
prop_playerHandSizeAfterHit game =
  let initialPlayerHand = playerHand game
   in length initialPlayerHand + 1 == length (playerHand (applyHit game))

-- P6 (Ao iniciar uma ronda, o player tem as 2 primeiras cartas e o dealer tem as 2 segundos cartas)
prop_cardDistribution :: EstadoJogo -> Bool
prop_cardDistribution game@EstadoJogo {deck} = playerHand gameAfterBet == take 2 deck && dealerHand gameAfterBet == take 2 (drop 2 deck)
  where
    gameAfterBet = applyBet game 1
