-- Ej 2
-- I
curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

-- II
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = \(x, y) -> f x y

-- III
-- No, las funciones en Haskell tienen estaticamente asignados los parametros.

-- Ej 3
-- [1, 3]
-- Son los numeros entre 1 y 3 que tienen numeros mayores a ellos pero menores a 3 cuya suma es congruente con 3.

-- Ej 4
-- Esta definicion no es util porque solo el primer valor va a cambiar, todos los otros van a ser siempre
-- 1, ya que estamos iterando por varias cosas infinitas.
pit = [(a, b, sqrt v) | v <- [1 ..], a <- [1 .. v], b <- [1 .. v], a ^ 2 + b ^ 2 == sqrt v]

-- Ej 5
primes = [x | x <- [2 ..], all ((0 /=) . (x `mod`)) [2 .. (x - 1)]]

-- primes = [x | x <- [2..], all (\n -> x `mod` n /= 0) [2..(x-1)]]

-- Ej 6
partir :: [a] -> [([a], [a])]
partir xs = [(take i xs, drop i xs) | i <- [0 .. length xs]]

-- Ej 7
lqs :: Int -> [[Int]]
lqs 0 = [[]]
lqs n = [(i : xs) | i <- [1 .. n], xs <- lqs (n - i)]

-- Ej 8
todasEnteros = [xs | i <- [1 ..], xs <- lqs (i)]

-- Ej 9
-- I
sum :: (Foldable t, Num a) => t a -> a
-- sum = foldr (\x r -> x + r) 0
sum = foldr (+) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem e = foldr (\x r -> r || (e == x)) False
elem e = foldr ((||) . (e ==)) False

-- (++) :: [a] -> [a] -> [a]
-- (++) xs ys = foldr (\x r -> (x:r)) ys xs
-- (++) xs ys = foldr (:) ys xs

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x r -> if f x then (x : r) else r) []

-- map :: (a -> b) -> [a] -> [b]
-- map f = foldr (\x r -> (f x : r)) []

-- II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x r -> if f x r then x else r)

-- III
sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = snd . foldl (\(acc, sec) x -> (acc + x, sec Prelude.++ [acc + x])) (0, [])

-- IV
sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

-- V
sumaAltInv :: (Num a) => [a] -> a
sumaAltInv = foldl (flip (-)) 0

-- VI
-- dropAt i xs = (init $ fst $ splitAt (i+1) xs) Prelude.++ (snd $ splitAt (i+1) xs)
dropAt i xs = take i xs Prelude.++ drop (i + 1) xs

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs =
    [ x : ys | i <- [0 .. (length xs) - 1], let x = xs !! i, ys <- perms (dropAt i xs)
    ]

-- Ej 11
epp :: [a] -> [a]
epp [] = []
epp (x : xs) = if null xs then [x] else x : epp (tail xs)

-- No usa recursion estructural ya que la recursion no esta siendo
-- aplicada sobre la cola, si no que sobre tail de la cola.

ent :: [a] -> [a] -> [a]
-- ent [] = id
-- ent (x:xs) = \ys -> if null ys then x:(ent xs ys)
--                     else x:head ys:ent xs (tail ys)

ent = foldr (\x r -> \ys -> if null ys then x : (r ys) else x : head ys : r (tail ys)) id

-- Ej 12
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- a
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else x:r) []

-- b
-- No tiene acceso al resto de la lista, entonces no podes saber si en el resultado de
-- la recursion no tenes el valor debido a que lo sacaste o debido a q no aparecio todavia.

-- c
insOrd :: Ord a => a -> [a] -> [a]
insOrd e = recr (\x xs r -> if x >= e then e:x:xs else x:r) []

-- d
-- No, la llamada recursiva es sobre un numero, no sobre un elemento de una lista

-- Ej 15
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\(x1,x2) r -> (f x1 x2 : r)) []

armarPares :: [a] -> [b] -> [(a, b)]
-- armarPares [] = const []
-- armarPares (x:xs) = \(y:ys) -> (x,y) : armarPares xs ys

armarPares = foldr (\x r (y:ys) -> if null ys then [(x,y)] else (x,y) : r ys) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f $ armarPares xs ys

-- Ej 17
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

-- TODO CONSULTAR
generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\xs -> if null xs then base else next $ last xs)

factoriales :: Int -> [Int]
factoriales n = generate (\l -> length l > n) (\xs -> if null xs then 1 else last xs * (length xs + 1))

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\l -> length l > n) x f

-- TODO CONSULTAR EL IV

-- Ej 18

-- TODO CONSULTAR
foldNat :: a -> (a -> a) -> Integer -> a
foldNat f g 0 = f
foldNat f g n = g (foldNat f g (n-1))

potencia :: Integer -> Integer -> Integer
potencia base = foldNat 1 (*base)

-- Ej 20
type Conj a = (a -> Bool)

vacio :: Conj a
vacio = const False

agregar :: Eq a => a -> Conj a -> Conj a
agregar e c = \x -> (x == e) || c x

interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 = \x -> c1 x && c2 x

union :: Conj a -> Conj a -> Conj a
union c1 c2 = \x -> c1 x || c2 x

inf :: Conj Int
inf = \x -> even x

singleton :: Eq a => a -> Conj a
singleton a = (== a)

-- No se puede definir map, ya que el dominio no es computable necesariamente.
-- Por ej, sea C conjunto de naturales que son los numeros de programa q
-- terminan, si map fuera computable entonces C tambien lo seria.

-- Ej 22
data AB a = Nil | Bin (AB a) a (AB a) deriving Show

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB leaf _ Nil = leaf
foldAB leaf tree (Bin t1 v t2) = tree (foldAB leaf tree t1) v (foldAB leaf tree t2)

-- TODO CONSULTAR
recAB :: b -> (AB a -> AB a -> b -> a -> b -> b) -> AB a -> b
recAB leaf _ Nil = leaf
recAB leaf tree (Bin t1 v t2) = tree t1 t2 (recAB leaf tree t1) v (recAB leaf tree t2)

esNil :: AB a -> Bool
esNil a = case a of
  Nil -> True
  Bin {} -> False

cantNodos :: AB a -> Int
cantNodos = foldAB 1 (\r1 x r2 -> 1 + r1 + r2)

-- TODO CONSULTAR
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f = recAB undefined mejor
  where
    mejor lt rt l x r
      | (esNil rt || f x r) && (esNil lt || f x l) = x
      | esNil rt || f x r                          = l
      | otherwise                                  = x

-- TODO CONSULTAR
esABB :: Ord a => AB a -> Bool
esABB = recAB True invariante
  where
    maxAB = mejorSegunAB (>)
    minAB = mejorSegunAB (<)
    invariante lt rt l x r
      | (esNil lt || maxAB lt <= x) && (esNil rt || x <= minAB rt) && l && r = True
      | otherwise                                                            = False

-- Ej 25
data RoseTree a = Rose a [RoseTree a]

exampleTree :: RoseTree Int
exampleTree = Rose 1
    [ Rose 2
        [ Rose 5 []
        , Rose 6 []
        ]
    , Rose 3
        [ Rose 7
            [ Rose 10 []
            , Rose 11 []
            ]
        ]
    , Rose 4
        [ Rose 8 []
        , Rose 9 []
        ]
    ]

foldT :: (a -> [b] -> b) -> RoseTree a -> b
foldT f (Rose a bs) = f a (map (foldT f) bs)

hojas :: RoseTree a -> [a]
hojas = foldT (\x rs -> if null rs then [x] else concat rs)

distancias :: RoseTree a -> [Int]
distancias = foldT (\x rs -> if null rs then [1] else map (+1) (concat rs))

altura :: RoseTree a -> Int
altura = foldT (\x rs -> if null rs then 1 else 1 + maximum rs)
