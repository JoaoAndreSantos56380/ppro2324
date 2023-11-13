import Data.Char (toLower)
import Data.List (sortBy)
import System.IO
import System.Environment
import System.Directory
--grupo 1

--era necessario importar o Data.Char para o 1º exemplo funcionar
--a)
encontra :: (a -> Bool) -> [a] -> Maybe a
encontra _ [] = Nothing
encontra f (x:xs)
    | f x = Just x
    | otherwise = encontra f xs

--b)
{-fatias :: Int -> [a] -> [[a]]
fatias 0 x = [x]
fatias _ [] = []
fatias n xs = take n xs : fatias n (drop n xs)
-}

--c)
--relembrar varias funcoes simples do prelude...
separa :: Eq a => [a] -> [a] -> [[a]]
separa [] a = [a]
separa _ [] = []
separa xs (y:ys)
    | y `elem` xs = separa xs ys
    | otherwise = word : separa xs (drop (length word) (y:ys))
        where word = takeWhile (\x -> not (x `elem` xs)) (y:ys)


--Grupo 2
--a)
data Doc = Vazio | Texto String | NovaLinha | Concat Doc Doc

instance Show Doc where
    show Vazio = ""
    show (Texto a) = a
    show NovaLinha = "\n"
    show (Concat a b) = show a ++ show b

instance Eq Doc where
    a == b = show a == show b


---------------------------------------Exame 2022 ---------------------------------------------
--exame 21\22 ex1
type Mapa k v = [(k, v)]

adicionar :: Eq k => k -> Mapa k Int -> Mapa k Int
adicionar k [] = [(k, 1)]
adicionar k ((k1, v1) : xs)
    | null xs && not (findKey k ((k1, v1) : xs)) = (k1, v1) : (k, 1) : xs
    | k == k1 = (k, v1 + 1) : xs
    | otherwise = (k1, v1) : adicionar k xs


findKey :: Eq k => k -> Mapa k a -> Bool
findKey k [] = False
findKey k ((k1, v1) : xs)
    | k == k1 = True
    | otherwise = findKey k xs

--Grupo 2
--a)

freq :: Eq k => [k] -> Mapa k Int
freq [] = []
freq xs = foldr adicionar [] xs

--b)
--funcao para substituir os caracteres de separacao por espacos para se poder usar a funcao words
subs :: String -> String
subs = foldr (\x acc -> if x `elem` stringSep then ' ' : acc else x : acc) []
    where stringSep = "\n\t,;.-!?"

frequencias :: String -> Mapa String Int
frequencias s = freq (words (subs s))

estrofe1 = "As armas e os barões assinalados,\nQue da ocidental praia Lusitana,\nPor mares nunca de antes navegados,\nPassaram ainda além da Taprobana,\nEm perigos e guerrasesforçados,\nMais do que prometia a força humana,\nE entregente remota edificaram\nNovo Reino, que tanto sublimaram;"

--c)
adicionarCom :: Eq k => (a -> a -> a) -> k -> a -> Mapa k a -> Mapa k a
adicionarCom _ k a [] = [(k, a)]
adicionarCom f k a ((k1, v1) : xs)
    | null xs && not (findKey k ((k1, v1) : xs)) = (k1, v1) : (k, a) : xs
    | k == k1 = (k, f v1 a) : xs
    | otherwise = (k1, v1) : adicionarCom f k a xs

--d) super weird
freq' :: Eq k => (a -> a -> a) -> [k] -> [a] -> Mapa k Int
freq' _ [] _ = []
freq' f xs ys = undefined--foldr (adicionarCom f a) [] xs

--e)
--funcao para substituir os caracteres de separacao por espacos para se poder usar a funcao words
subs2 :: String -> String
subs2 = foldr (\x acc -> if x `elem` stringSep then ' ' : acc else toLower x : acc) []
    where stringSep = "\n\t,;.-!?"

frequencias2 :: String -> Mapa String Int
frequencias2 s = freq (filter (\x -> length x > 2) (words (subs2 s)))


--f)
primeirasFrequencias :: Int -> String -> Mapa String Int
primeirasFrequencias n s = take n (sortBy sortMap (frequencias2 s))

sortMap :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
sortMap (_, v1) (_, v2) = compare v2 v1

destrava = "Se o papa papasse papa, se o papa papasse pão, se o papa tudo papasse, seria um papa-papão."


--Grupo 4
--a)
data Exp = ConstInt Int | Mais Exp Exp | ConstBool Bool | Ou Exp Exp | If Exp Exp Exp

instance Show Exp where
    show (ConstInt n) = show n
    show (Mais exp1 exp2) = show exp1 ++ " + " ++ show exp2
    show (ConstBool a) = show a
    show (Ou a b) = show a ++ " || " ++ show b
    show (If a b c) = "If " ++ show a ++ " then " ++ show b ++ " else " ++ show c

data Tipo = Inteiro | Booleano deriving (Eq, Show)


--very weird toooo
tipoDe :: Exp -> Tipo
tipoDe = undefined

{--
main :: IO ()
main = do
       x <- getArgs
       handle <- openFile (head x) ReadMode
       contents <- hGetContents handle
       --let text = words contents
       let freque = primeirasFrequencias n contents
       hClose handle
       return ()
       --}


-- Exame 2

--Grupo 1
--a)
numeros :: String -> Bool
numeros s = foldr (\x -> if x `elem` ['0','1','2','3','4','5','6','7','8','9'] then (True &&) else (False &&)) True s


--b)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) 
    | x `elem` xs = x : nub (remove x xs)
    | otherwise = x : nub xs

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
    | x == y = remove x ys
    | otherwise = y : remove x ys

--c)



--Grupo 2
type NomeDisciplina = String
type Nota = Int
type Disciplina = (NomeDisciplina, Nota)
data Aluno = Aluno {nomeAluno :: String,
                    numeroAluno :: Int,
                    disciplinas :: [Disciplina]}

ds :: [Disciplina]
ds = [("Português", 9), ("Matemática", 15),("História", 13)]
joao :: Aluno
joao = Aluno {nomeAluno = "João", numeroAluno = 12345,
disciplinas = ds}



--a)
aprovadas :: Aluno -> Int
aprovadas aluno = contaNotas (disciplinas aluno)

contaNotas :: [(a, Int)] -> Int
contaNotas [] = 0
contaNotas ((_, x):xs)
    | x >= 10 = contaNotas xs + 1
    | otherwise = contaNotas xs

--b)
passou :: Aluno -> Bool
passou aluno = contaNotas (disciplinas aluno) == length (disciplinas aluno)

--c) ??????????????????
media :: Aluno -> Float
media aluno = undefined --somaNotas (disciplinas aluno) `div` length (disciplinas aluno)

somaNotas :: [(a, Int)] -> Int
somaNotas [] = 0
somaNotas ((_, x):xs) = x + somaNotas xs

instance Show Aluno where
    show a = show (numeroAluno a) ++ " " ++ show (length (disciplinas a))

--Grupo 3
--a)

data Arvore a = Folha a | No (Arvore a) (Arvore a)

contaMaiores :: Int -> Arvore Int -> Int
contaMaiores n (Folha value) 
    | value >= n = 1
    | otherwise = 0
contaMaiores n (No left right) = contaMaiores n left + contaMaiores n right

instance Foldable Arvore where
    foldr f acc (Folha x) = f x acc
    foldr f acc (No esquerda direita) = foldr f (foldr f acc direita) esquerda

contaMaiores' :: Int -> Arvore Int -> Int
contaMaiores' n arvore = foldr (\x -> if x >= n then (1 +) else (0 +)) 0 arvore

--2020 2fase
data Metro = Metro Int deriving Show
type Linha = String
data Alocacao = Alocacao [(Metro, Linha)] [Metro] deriving Show

existeMetroLivre :: Int -> Alocacao -> Bool
existeMetroLivre _ (Alocacao _ []) = False
existeMetroLivre n (Alocacao m ((Metro x):xs))
    | x >= n = True
    | otherwise = existeMetroLivre n (Alocacao m xs)


alocaMetro :: Int -> Linha -> Alocacao -> Alocacao
alocaMetro _ [] (Alocacao m x) = (Alocacao m x)
alocaMetro _ _ (Alocacao m []) = (Alocacao m [])
alocaMetro n l (Alocacao m ((Metro x):xs))
    | x >= n = Alocacao (((Metro x), l):m) xs
    | otherwise = alocaMetro n l (Alocacao m (xs++[Metro x]))


--2019 1a fase
--grupo 2
data Html = Div [Html] | Text String | Negrito String

profundidade :: Html -> Int
profundidade (Text s) = 1
profundidade (Negrito g) = 1
profundidade (Div []) = 1
profundidade (Div [x]) = 1
profundidade (Div (x:xs)) = 1 + profundidade x + profundidade (Div xs)--foldr (\x acc -> acc + profundidade x) 1 xs

instance Show Html where
    show (Text s) = show s
    show (Negrito g) = "<b>" ++ show g ++ "</b>"
    show (Div d) = "<div>" ++ show d ++ "<div>"

--Exame 2023

--a)
posicoesImpares :: [a] -> [a]
posicoesImpares [] = []
posicoesImpares [x] = [x]
posicoesImpares (x:_:xs) = x : posicoesImpares xs

--b)
fatias :: Int -> [a] -> [[a]]
fatias _ [] = []
fatias n xs = take n xs : fatias n (drop n xs)

data Numero = Zero | Succ Numero | Prod Numero Numero

avalia :: Numero -> Int
avalia Zero = 0
avalia (Succ n) = 1 + avalia n
avalia (Prod a b) = avalia a * avalia b

instance Eq Numero where
    num1 == num2 = avalia num1 == avalia num2
