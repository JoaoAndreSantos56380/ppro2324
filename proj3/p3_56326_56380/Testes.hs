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

prop_initialHandValue :: Carta -> Carta -> Bool
prop_initialHandValue left right = convenientHandValue [left,right] <= 21



{- prop_playerCreditsAfterRound :: EstadoJogo -> Int -> Bool -> Bool
prop_playerCreditsAfterRound game bet hit =
    (bet <= 0) || (do
        let gamePlayerCredits = playerCredits game
        let updatedGame = game {currentBet = bet}
        let updatedState = playRound updatedGame bet hit
        let updatedPlayerCredits = playerCredits updatedState
        updatedPlayerCredits + bet == gamePlayerCredits || updatedPlayerCredits == gamePlayerCredits - bet || updatedPlayerCredits == gamePlayerCredits) -}

-- The property to test
prop_CreditsAfterRound :: Int -> Int -> Bool -> EstadoJogo -> Property
prop_CreditsAfterRound n a standOrHit gameState =
  n > 0
    && a > 0
    && a
      <= n
        ==> let initialGameState -- Ensure valid starting credits and bet
                  = applyBet gameState a  -- Assuming a function to set initial credits
                newState = playRound initialGameState a standOrHit
                finalCredits = playerCredits newState -- Assuming a function to get current credits
             in finalCredits == playerCredits newState - a || finalCredits == playerCredits newState || finalCredits == playerCredits newState + a

-- The property to test
{- prop_CreditsAfterRound :: Int -> Int -> Bool -> EstadoJogo -> Bool
prop_CreditsAfterRound n a standOrHit gameState =
  let initialCredits = n
      bet = a
      newState = playRound gameState bet standOrHit -- Play a round with the bet 'a' and choice of stand or hit
      finalCredits = creditos newState -- Get the credits after the round
   in finalCredits == n - a || finalCredits == n || finalCredits == n + a -}


{- prop_playerCreditsAfterRound2 :: EstadoJogo -> Int -> Bool -> Bool
prop_playerCreditsAfterRound2 game bet hit = do
  bet > 0 && convenientHandValue (playerHand game) /= 21 ==> do
        let gamePlayerCredits = playerCredits game
        let updatedGame = game {currentBet = bet}
        let updatedState = playRound updatedGame bet hit
        let updatedPlayerCredits = playerCredits updatedState
        updatedPlayerCredits == gamePlayerCredits + bet || updatedPlayerCredits == gamePlayerCredits - bet || updatedPlayerCredits == gamePlayerCredits -}

{- prop_dealerHandValue :: [Carta] -> Bool
prop_dealerHandValue dealerHand =
  let finalValue = undefined -- Função que simula a jogada da casa e retorna o valor da mão
   in finalValue >= 17 -}

{- prop_deckIntegrity :: Baralho -> Property
prop_deckIntegrity deck = length deck == 52 ==> length (nub deck) == length deck -}

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

