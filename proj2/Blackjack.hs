-- fc56326,fc56380
module Blackjack (
converte,
tamanho{- , inicializa, creditos, baralho, terminado, sempreStand, sempreHit, simulaRonda, simulaJogo -}) where
import BaralhosExemplo

data Naipe = Copas | Ouros | Paus | Espadas deriving (Show, Eq)
data Valor = Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei | As deriving (Show, Eq)
data Carta = Carta Valor Naipe deriving (Show, Eq)

type Baralho = [Carta]

converte :: [String] -> Baralho
converte = map stringToCard
  where
    stringToCard [r, s] = Carta (charToValor r) (charToNaipe s)
    stringToCard _      = error "Cartada mal feita"

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
    charToValor _   = error "Valor invalido"

    charToNaipe 'H' = Copas
    charToNaipe 'C' = Paus
    charToNaipe 'D' = Ouros
    charToNaipe 'S' = Espadas
    charToNaipe _   = error "Naipe invalido"


tamanho :: Baralho -> Int
tamanho xs = length xs



