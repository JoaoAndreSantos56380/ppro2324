{-
Princípios de Programação 2022/2023
Licenciatura em Engenharia Informática
Faculdade de Ciências
Universidade de Lisboa

Autores:
    Diogo Poças
    Alcides Fonseca

Este é o oitavo trabalho para casa da disciplina de Princípios de Programação 2022/2023.

Endereça os seguintes tópicos: tipos e classes de tipos.

Para completar o trabalho basta preencher com código Haskell os espaços em falta.

O resultado será discutido na aula da semana de 1 de novembro, não sendo necessário submeter para avaliação.
-}
{-# LANGUAGE InstanceSigs #-}
module Ex_08 where

{- 5. Considere o tipo de dados árvores de pesquisa. -}

data Tree a = EmptyTree | Node (Tree a) a (Tree a)

{- Assuma que numa árvore de pesquisa, os valores estão ordenados: o valor de cada 
nó é maior ou igual a cada valor da sub-árvore esquerda, e o valor de cada nó é 
menor ou igual que cada valor da sub-árvore direita. Escreva as funções abaixo. -}

{- (c) size :: Tree a -> Int, o número de nós na árvore. -}
size :: Tree a -> Int
size EmptyTree = 0
size (Node a _ b) = 1 + size a + size b

{- (d) depth :: Tree a -> Int, a profundidade da árvore. A profundidade de uma 
árvore vazia é zero; aquela de uma árvore não vazia é um mais o máximo das 
profundidades das sub-árvores. -}
depth :: Tree a -> Int
depth EmptyTree = 0
depth (Node x _ z) = 1 + max (depth x) (depth z)


{- (e) isPerfect :: Tree a -> Bool, a árvore é perfeita? Uma árvore vazia é 
considerada perfeita. Uma árvore não vazia diz-se perfeita se as duas sub-árvores
são perfeitas e têm o mesmo número de nós. Numa primeira fase, resolva este 
exercício recorrendo à função size. Analise a sua complexidade. Desenhe depois 
uma solução que não percorra a árvore mais do que uma vez. -}
isPerfect :: Tree a -> Bool
isPerfect EmptyTree = True
isPerfect (Node x _ z) = (size x == size z) && isPerfect x && isPerfect z

--very smort... uma arvore so e perfeita se o seu size for 0 2 4 8 16... estes elementos todos sao as potencias de 2
isPerfect':: Tree a -> Bool
isPerfect' EmptyTree = True
isPerfect' tree = size tree `elem` [2^x | x <- [0..]]

{- (h) toList :: Tree a -> [a], a lista dos elementos da árvore visitados pelo 
percurso infixo. O percurso infixo de uma árvore não vazia visita primeiro a 
sub-árvore esquerda, depois o elemento do nó e finalmente a sub-árvore direita. -}
toList :: Tree a -> [a]
toList EmptyTree = []
toList (Node x y z) = y : (toList x ++ toList z)


{- 7. Torne o tipo de dados Tree instância da classe Eq. Para efeitos deste 
exercício duas árvores são iguais se contiverem os mesmos elementos. -}
instance Eq a => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    tree1 == tree2 = allBelong tree1 tree2


--se uma arvore é igual a outra ou se é igual a uma das suas sub arvores
belongsToTree :: Eq a => Tree a -> a -> Bool
belongsToTree EmptyTree _ = False
belongsToTree (Node x y z) a = a == y || belongsToTree x a || belongsToTree z a

allBelong :: Eq a => Tree a -> Tree a -> Bool
allBelong _ EmptyTree = False
allBelong EmptyTree _ = True
allBelong (Node x y z) tree = belongsToTree tree y && allBelong x tree && allBelong z tree




{- 9. A classe de tipos Foldable a aplica-se a todos os tipos que possam ser 
percorridos na sua totalidade, para produzir um dado valor. Para tornar a 
classe Tree a (definida no exercício 5) instância da classe de tipos Foldable a
basta escrever uma função com a seguinte assinatura.
foldr :: (a -> b -> b) -> b -> Tree a -> b
Depois ganhamos automaticamente uma série de funções derivadas do foldr, incluindo
foldl, foldr1, foldl1, null, length, elem, maximum. minimum, sum, product -}

{- (a) Torne o tipo de dados Tree a instância da classe de tipos Foldable. Para
isso basta escrever uma função com a seguinte assinatura.
foldr :: (a -> b -> b) -> b -> Tree a -> b
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
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    --foldr _ acc EmptyTree = acc
    --foldr f acc (Node x y z) = f y (foldr acc f z (foldr acc f x))
    foldr _ z EmptyTree = z
    foldr f z (Node EmptyTree x  EmptyTree) = f x z
    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

{- (b) Reescreva a função length usando a função foldr. -}
length' :: a
length' = undefined

{- (d) Reescreva a função sum usando a função foldr. -}
sum' = undefined

{- (e) Escreva uma função toListR :: Tree a -> [a] que devolve a lista de 
elementos na árvore pelo sentido infixo-inverso. -}
toListR = undefined


--exame 21\22 ex1
type Mapa k v = [(k, v)]

adicionar :: Eq k => k -> Mapa k Int -> Mapa k Int
adicionar k [] = []
adicionar k ((k1, v1) : xs)
    | null xs && not (findKey k ((k1, v1) : xs)) = (k1, v1) : (k, 1) : adicionar k []
    | k == k1 = (k, v1 + 1) : adicionar k xs
    | otherwise = (k1, v1) : adicionar k xs


findKey :: Eq k => k -> Mapa k Int -> Bool
findKey k [] = False
findKey k ((k1, v1) : xs)
    | k == k1 = True
    | otherwise = findKey k xs