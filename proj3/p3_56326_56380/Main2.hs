import Blackjack
import Data.Type.Equality (apply)
main = do
    let game = EstadoJogo { playerCredits = 2, currentBet = 0, deck = converte [['T','S'], ['9','S'], ['5','S'], ['9','H'], ['T','S']] ++ converte [[valor, naipe] | naipe <- "SHDC", valor <- "A23456789TJQK"], playerHand = [], dealerHand = [], state = Initial}
    print (playRound (applyBet game 1) 1 True)

