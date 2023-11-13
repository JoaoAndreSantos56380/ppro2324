{-
Princípios de Programação 2022/2023
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o sétimo trabalho para casa da disciplina de Princípios de Programação 2022/2023.

Endereça os seguintes tópicos: módulos.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido na aula da semana de 31 de outubro, não sendo necessário submeter para avaliação.
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

import Prelude hiding (null, lookup)
import qualified Prelude as P (null, lookup)

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
singleton k v = M [(k, v)]

-- null, verificar se um mapa está vazio,
null :: Map k v -> Bool
null m = size m == 0

-- size, o número de elementos no mapa,
size :: Map k v -> Int
size (M xs)= length xs

-- member, verificar se uma chave está no mapa,
member :: Ord k => k -> Map k v -> Bool
member _ (M []) = False
member key (M ((k, _):xs)) = (k == key) || member key (M xs)

-- lookup, procurar uma chave no mapa, obtendo o valor associado (Just valor),
-- ou Nothing, caso contrário,
lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ (M []) = Nothing
lookup key (M ((k,v):xs)) = if key == k then Just v
                                        else lookup key (M xs)

-- insert, juntar uma entrada (chave, valor) ao mapa, substituindo o valor 
-- caso a chave já esteja no mapa,
insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (M xs) = M (insertAux (k,v) xs)

insertAux :: Ord k => (k,v) -> [(k,v)] -> [(k,v)]
insertAux (k,v) [] = [(k,v)]
insertAux (k,v) ((k1,v1):xs) 
    | k < k1  = (k,v):(k1,v1):xs
    | k == k1 = (k,v):xs
    | otherwise = (k1,v1):insertAux (k,v) xs

-- delete, apagar uma chave e o seu valor de um mapa,
delete :: Ord k => k -> Map k v -> Map k v
delete k (M xs) = M (deleteAux k xs)

deleteAux :: Ord k => k -> [(k,v)] -> [(k,v)]
deleteAux _ [] = []
deleteAux k ((key,v):xs)
    | k == key = xs
    | otherwise = (key,v):deleteAux k xs

-- unionWith, unir dois mapas, utilizando uma função para combinar os 
-- valores de chaves duplicadas,
unionWith :: Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (M xs) (M ys) = M (unionWithAux f xs ys)

unionWithAux :: Ord k => (v -> v -> v) -> [(k,v)] -> [(k,v)] -> [(k,v)]
unionWithAux _ [] ys = ys
unionWithAux _ xs [] = xs
unionWithAux f ((k1,v1):xs) ((k2,v2):ys)
    | k1 == k2  = (k1, f v1 v2):unionWithAux f xs ys
    | k1 < k2 = (k1,v1) : unionWithAux f xs ((k2,v2):ys)
    | otherwise = (k2,v2):unionWithAux f ((k1,v1):xs) ys

-- fromList, criar um mapa a partir de uma lista de pares (não ordenada),
fromList :: Ord k => [(k, v)] -> Map k v
fromList = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

fromListAux :: Ord k => [(k, v)] -> Map k v
fromListAux xs = M xs

-- toList, para obter uma lista com os pares constantes num mapa.
toList :: Map k v -> [(k, v)]
toList (M xs) = xs


findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v):xs) = if key == k
                            then Just v
                            else findKey key xs