{-
Princípios de Programação
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o oitavo trabalho para casa da disciplina de Princípios de Programação.

Endereça os seguintes tópicos: tipos e classes de tipos.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido nas aulas teórico-práticas, não sendo necessário submeter para avaliação.
-}
module Ex_08 where

{- 5. Considere o tipo de dados árvores de pesquisa. -}

data Arvore a = Folha | No (Arvore a) a (Arvore a) -- deriving (Eq, Ord, Show, Read)

{- Assuma que numa árvore de pesquisa, os valores estão ordenados: o valor de
cada nó é maior ou igual a cada valor da sub-árvore esquerda, e o valor de
cada nó é menor ou igual que cada valor da sub-árvore direita. Escreva as
funções abaixo. -}

{- (a) empty :: Arvore a, uma árvore vazia. -}
{- (a) empty :: Arvore a, uma árvore vazia. -}
empty = undefined

{- (b) singleton :: a -> Arvore a, uma árvore com um elemento. -}
singleton :: a -> Arvore a
singleton x = No Folha x Folha

{- (c) size :: Arvore a -> Int, o número de nós na árvore. -}
size :: Arvore a -> Int
size Folha = 0
size (No left _ right) = 1 + size left + size right

{- (d) depth :: Arvore a -> Int, a profundidade da árvore. A profundidade de uma
árvore vazia é zero; aquela de uma árvore não vazia é um mais o máximo das
profundidades das sub-árvores. -}
depth :: Arvore a -> Int
depth Folha = 0
depth (No left _ right) = max (depth left) (depth right)

{- (e) isPerfect :: Arvore a -> Bool, a árvore é perfeita? Uma árvore vazia é
considerada perfeita. Uma árvore não vazia diz-se perfeita se as duas
sub-árvores são perfeitas e têm o mesmo número de nós. Numa primeira fase,
resolva este exercício recorrendo à função size. Analise a sua complexidade.
Desenhe depois uma solução que não percorra a árvore mais do que uma vez. -}
isPerfect :: Arvore a -> Bool
isPerfect (No left x right) = (size left) == (size right) && isPerfect left && isPerfect right

isPerfect' :: Arvore a -> Bool
isPerfect' tree = allEqual $ distanceList tree 0

-- size tree == 2^(depth tree) - 1
distanceList:: Arvore a -> Int -> [Int]
distanceList Folha n = [n]
distanceList (No left _ right) n = distanceList left (n+1) ++ distanceList right (n+1)

allEqual:: [Int] -> Bool
allEqual xs = foldl( \acc x -> x == head xs && acc) True xs
{- (f) insert :: Ord a => a -> Arvore a -> Arvore a, insere um elemento numa
árvore de pesquisa (mantendo a ordenação). -}
insert = undefined

{- (g) fromList :: Ord a => [a] -> Arvore a, uma árvore sintetizada a partir de
uma lista de elementos (não ordenada). -}
fromList = undefined

{- (h) toList :: Arvore a -> [a], a lista dos elementos da árvore visitados
pelo percurso infixo. O percurso infixo de uma árvore não vazia visita
primeiro a sub-árvore esquerda, depois o elemento do nó e finalmente a
sub-árvore direita. -}
toList :: Arvore a -> [a]
toList (No esq a dir)= toList esq ++ [a] ++ toList dir

{- (i) elem :: Ord a =>a -> Arvore a -> Bool, verifica se um dado elemento
consta de uma árvore. -}
elem = undefined

{- (j) allIn :: Ord a => Arvore a -> Arvore a -> Bool, verifica se todos os
elementos de uma dada árvore constam de uma outra árvore. -}
allIn = undefined


{- 6. Reescreva as alíneas c, d, e, h, i, j do exercício 5 recorrendo à
seguinte função fold. -}
fold :: b -> (b -> a -> b -> b) -> Arvore a -> b
fold e _ Folha    = e
fold e f (No x n y) = f (fold e f x) n (fold e f y)

-- (c) size
size' = undefined

-- (d) depth
depth' = undefined

-- (e) isPerfect
isPerfect'' = undefined

-- (h) toList
toList'' = undefined

-- (i) elem
elem' = undefined

-- (j) allIn
allIn3 = undefined


{- 7. Torne o tipo de dados Tree a instância da classe Eq. Para efeitos deste
exercício duas árvores são iguais se contiverem os mesmos elementos. -}
instance Eq a => Eq (Arvore a) where
    {- (==) =  -}arvore1 == arvore2 = toList arvore1 == toList arvore2


{- 8. Torne o tipo de dados Arvore a instância da classe Show. A conversão de
uma árvore numa String deverá ser tal que a árvore

Node (Node Folha "cao" (Node Folha "gato" Folha)) "peixe" (Node Folha "pulga" Folha)

seja convertida em

"peixe"
  "cao"
    Folha
    "gato"
      Folha
      Folha
  "pulga"
    Folha
    Folha
-}

instance Show a => Show (Arvore a) where
  show = undefined

{- 9. A classe de tipos Foldable a aplica-se a todos os tipos que possam ser
percorridos na sua totalidade, para produzir um dado valor. Para tornar a
classe Arvore a (definida no exercício 5) instância da classe de tipos Foldable a
basta escrever uma função com a seguinte assinatura.
foldr :: (a -> b -> b) -> b -> Arvore a -> b
Depois ganhamos automaticamente uma série de funções derivadas do foldr, incluindo
foldl, foldr1, foldl1, null, length, elem, maximum. minimum, sum, product -}

{- (a) Torne o tipo de dados Arvore a instância da classe de tipos Foldable. Para
isso basta escrever uma função com a seguinte assinatura.
foldr :: (a -> b -> b) -> b -> Arvore a -> b
A função deverá percorrer a árvore no sentido infixo-inverso, isto é: primeiro
percorre a subárvore direita, depois a raiz e finalmente a subárvore esquerda.
> t = fromList [3, 6, 8, 0, 7, 9, 2]
> foldr (:) [] t
[0,2,3,6,7,8,9]
> foldl (flip (:)) [] t
[9,8,7,6,3,2,0]
> sum t
35
> maximum t
9
-}
instance Foldable Arvore where
  --foldr :: (a -> b -> b) -> b -> Arvore a -> b
  foldr _ acc Folha = acc
  foldr f acc (No left key right) = foldr f (f key (foldr f acc right) left) -- nao esta bem

{- (b) Reescreva a função length usando a função foldr. -}
length' :: (Arvore a) -> Int
length' tree = foldr (\x acc -> acc + 1) 0 tree

{- (c) Reescreva a função elem usando a função foldr. -}
elem'':: Eq a => a ->Arvore a -> Bool
elem'' val = foldr (\x acc -> val == x || acc) False

{- (d) Reescreva a função sum usando a função foldr. -}
sum' :: (Arvore a) -> a
sum' = foldr (+) 0

{- (e) Escreva uma função toListR :: Arvore a -> [a] que devolve a lista de
elementos na árvore pelo sentido infixo-inverso. -}
toListR :: Arvore a -> [a]
toListR tree = foldr (\x acc -> acc ++ [x]) [] tree

{- (f) Escreva uma função toListL :: Arvore a -> [a] que devolve a lista de
elementos na árvore pelo sentido infixo convencional. -}
toListL = undefined

{- 10. Torne o tipo Tree instância da class Functor. -}
instance Functor Arvore where
  fmap = undefined
