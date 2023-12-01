import Blackjack
import BaralhosExemplo
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

