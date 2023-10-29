{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o sétimo trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: módulos.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}

module Map
( Map(..)
, empty
, singleton
, null
, size
, member
, lookup
, insert
, delete
, unionWith
, fromList
, toList
) where

import Prelude hiding (null, lookup, length)
import qualified Prelude as P (null, lookup, length)

{- 2. Escreva um módulo que represente um mapa: uma estrutura de dados que
mantém associações entre chaves e valores, onde as chaves não devem aparecer
repetidas. Defina um tipo de dados para mapas com chaves do tipo k e valores
do tipo v. -}

newtype Map k v = M [(k, v)] deriving Show

{- Utilize uma representação baseada em listas de associação, isto é, um mapa
deverá ser representado por uma lista de pares chave-valor. Prepare as
seguintes operações do módulo, que deverão manter duas invariantes:
    (a) As chaves não aparecem repetidas,
    (b) A lista está ordenada pelas chaves. -}

-- empty, o mapa vazio,
empty :: Map k v
empty = M []

-- singleton, construir um mapa com um único elemento,
singleton :: k -> v -> Map k v
singleton k v = M [(k,v)]

-- null, verificar se um mapa está vazio,
null :: Map k v -> Bool
null (M kvPairs) = P.null kvPairs


-- size, o número de elementos no mapa,
size :: Map k v -> Int
size (M kvPairs) = P.length kvPairs

-- member, verificar se uma chave está no mapa,
member :: Ord k => k -> Map k v -> Bool
member = undefined

-- lookup, procurar uma chave no mapa, obtendo o valor associado (Just valor),
-- ou Nothing, caso contrário,
lookup :: Ord k => k -> Map k v -> Maybe v
lookup = undefined

-- insert, juntar uma entrada (chave, valor) ao mapa, substituindo o valor
-- caso a chave já esteja no mapa,
insert :: Ord k => k -> v -> Map k v -> Map k v
insert = undefined

-- delete, apagar uma chave e o seu valor de um mapa,
delete :: Ord k => k -> Map k v -> Map k v
delete = undefined

-- unionWith, unir dois mapas, utilizando uma função para combinar os
-- valores de chaves duplicadas,
unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith = undefined

-- fromList, criar um mapa a partir de uma lista de pares (não ordenada),
fromList :: Ord k => [(k, v)] -> Map k v
fromList = undefined

-- toList, para obter uma lista com os pares constantes num mapa.
toList :: Map k v -> [(k, v)]
toList = undefined
