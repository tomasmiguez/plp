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
