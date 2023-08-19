import Control.Arrow
import Data.List (genericLength)
-----------------------------------------------------------------------
-- Ejercicio 1
-----------------------------------------------------------------------
-- >>> :t null
-- null :: Foldable t => t a -> Bool
-- >>> null []
-- True
-- >>> null [1]
-- False
-- >>> null [1..]
-- False

-- >>> :t head
-- head :: [a] -> a
-- >>> head []
-- *** Exception: Prelude.head: empty list
-- >>> head [1]
-- 1
-- >>> head [1,2]
-- 1

-- >>> :t last
-- last :: [a] -> a
-- >>> last []
-- *** Exception: Prelude.last: empty list
-- >>> last [1]
-- 1
-- >>> last [1,2]
-- 2

-- >>> :t tail
-- tail :: [a] -> [a]
-- >>> tail []
-- *** Exception: Prelude.tail: empty list
-- >>> tail [1]
-- []
-- >>> tail [1,2]
-- [2]

-- >>> :t init
-- init :: [a] -> [a]
-- >>> init []
-- *** Exception: Prelude.init: empty list
-- >>> init [1]
-- []
-- >>> init [1,2]
-- [1]

-- >>> :t take
-- take :: Int -> [a] -> [a]
-- >>> take 0 []
-- []
-- >>> take 1 [1]
-- [1]
-- >>> take 1 [1,2]
-- [1]

-- >>> :t drop
-- drop :: Int -> [a] -> [a]
-- >>> drop 0 []
-- []
-- >>> drop 1 [1]
-- []
-- >>> drop 1 [1,2]
-- [2]

-- >>> :t (++)
-- (++) :: [a] -> [a] -> [a]
-- >>> [1] ++ []
-- [1]
-- >>> [2] ++ [1]
-- [2,1]

-- >>> :t concat
-- concat :: Foldable t => t [a] -> [a]
-- >>> concat []
-- []
-- >>> concat [[1]]
-- [1]
-- >>> concat [[1],[2]]
-- [1,2]

-- >>> :t (!!)
-- (!!) :: [a] -> Int -> a
-- >>> [1] !! 0
-- 1
-- >>> [1,2] !! 0
-- 1
-- >>> [1,2] !! 1
-- 2
-- >>> [1,2] !! 2
-- *** Exception: Prelude.!!: index too large
-- >>> [1,2] !! (-1)
-- *** Exception: Prelude.!!: negative index

-- >>> :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- >>> elem 1 []
-- False
-- >>> elem 1 [1]
-- True

-----------------------------------------------------------------------
-- Ejercicio 2
-----------------------------------------------------------------------
-- a
valorAbsoluto :: Float -> Float
valorAbsoluto x | x >= 0    = x
                | otherwise = -x

-- b
bisiesto :: Int -> Bool
bisiesto x | mod x 400 == 0 = True
           | mod x 100 == 0 = False
           | mod x 4 == 0   = True
           | otherwise      = False

-- c
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- d
contarDivisoresPrimosHasta :: Int -> Int -> Int
contarDivisoresPrimosHasta x 1 = 1
contarDivisoresPrimosHasta x y | (mod x y) == 0 = contarDivisoresPrimosHasta x (y - 1) + 1
                               | otherwise      = contarDivisoresPrimosHasta x (y - 1)

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = contarDivisoresPrimosHasta x x

-----------------------------------------------------------------------
-- Ejercicio 3
-----------------------------------------------------------------------
-- a
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

-- b
aEntero :: Either Int Bool -> Int
aEntero (Right x) | x         = 1
                  | otherwise = 0
aEntero (Left x) = x

-----------------------------------------------------------------------
-- Ejercicio 4
-----------------------------------------------------------------------
-- a
limpiarCaracter :: Char -> String -> String
limpiarCaracter c [] = []
limpiarCaracter c (x:xs) | (c == x)  = limpiarCaracter c xs
                         | otherwise = x:(limpiarCaracter c xs)

limpiar :: String -> String -> String
limpiar [] s = s
limpiar (x:xs) s = limpiar xs (limpiarCaracter x s)

-- b
-- (⊢-(+/÷⍴))

promedio :: [Float] -> Float
promedio = uncurry (/) . (sum &&& genericLength)
-- promedio xs = (sum xs) / genericLength xs

difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio xs) xs

-- c
-- ∧/(1∘↑=⊢)

-- all :: Foldable t => (a -> Bool) -> t a -> Bool
-- (==) . head :: Eq b => [b] -> b -> Bool
--
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
-- f =<< g = \x -> f (g x) x
-- (=<<) :: (a -> r -> b) -> (r -> a) -> (r -> b)
-- a = (a -> Bool)
-- r = [a]
-- b = Bool
-- (=<<) :: ((a -> Bool) -> [a] -> Bool) -> ([a] -> a -> Bool) -> [a] -> Bool
--
-- all =<< (==) . head :: Eq a => [a] -> Bool

todosIguales :: [Int] -> Bool
todosIguales = all =<< (==) . head

-----------------------------------------------------------------------
-- Ejercicio 5
-----------------------------------------------------------------------
data AB a = Nil | Bin (AB a) a (AB a) deriving Show

-- a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l x r) = Bin (negacionAB l) (not x) (negacionAB r)

-- c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l x r) = (productoAB l) * x * (productoAB r)
