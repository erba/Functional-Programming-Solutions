import Data.Char    -- für digitToInt

-- chap 2 slide 50
scalar :: Num a => [a] -> [a] -> a
scalar ls1 [] = 0
scalar [] ls2 = 0
-- scalar ls1 ls2 = ((head ls1) * (head ls2)) + (scalar (tail ls1) (tail ls2))
scalar (x1:xs1) (x2:xs2) = (x1 * x2) + (scalar xs1 xs2)

scalarGuarded :: Num a => [a] -> [a] -> a
scalarGuarded ls1 ls2   | length ls1 == 0 = 0
                        | length ls2 == 0 = 0
                        | otherwise = ((head ls1) * (head ls2)) + (scalar (tail ls1) (tail ls2))

scalarSimple :: Num a => [a] -> [a] -> a
scalarSimple ls1 ls2 =  sum [a*b | (a,b)<-(zip ls1 ls2)]

-------------------
-- Exercise Sheet 1
-------------------
---------------
-- EXERCISE 1.1
---------------
-- 1.1.1
double :: Integer -> Integer
double x = 2 * x

-- 1.1.2
ratio :: Float -> Float -> Float
ratio x y = (x+y) / (x-y)

-- 1.1.3
hypotenuse :: Double -> Double -> Double
hypotenuse x y = sqrt (x^2 + y^2)

-- 1.1.4
xIntercept :: Integer -> Integer -> Float
xIntercept m c = fromInteger (-c) / fromInteger m

-- 1.1.5
threeDiff :: Integer -> Integer -> Integer -> Bool
threeDiff m n p | m /= n && n /= p && m /= p = True
                | otherwise = False

-- 1.1.6
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromInteger (a + b + c)) / 3

-- 1.1.7
arithmeticSum :: Fractional a => a -> a -> a -> a
arithmeticSum a n d = n * (2*a + (n-1)*d) / 2

-- 1.1.8
inrange1 :: Float -> Float -> Float -> Bool
inrange1 x a b | a <= x && x <= b = True
               | b <= x && x <= a = True
               | otherwise = False

-- 1.1.9
exclusiveOr :: Bool -> Bool -> Bool
exclusiveOr x y | x /= y = True
                | otherwise = False

-- 1.1.10
implies :: Bool -> Bool -> Bool
implies False _ = True
implies _ True = True
implies _ _ = False

-- 1.1.11
hundreds :: Int -> Int
hundreds a | a >= 100 = digitToInt(lst !! (length lst - 3))
           | otherwise = 0
           where lst = show a

-- 1.1.12
middle :: String -> Maybe Char
middle [] = Nothing
middle (x:"") = Just x
-- middle str = Just (str !! (floor ((fromIntegral(length str)) / 2)))
middle str = Just $ str !! (floor $ (fromIntegral $ length str) / 2)

---------------
-- EXERCISE 1.2
---------------
-- 1.2.1
-- func :: [a] -> Int
--
-- 1.2.2
-- func :: Char -> Char
--
-- 1.2.3
-- func :: String -> String -> Bool    works but
-- func :: [a] -> [a] -> Bool     is the better solution

---------------
-- EXERCISE 1.3
---------------
-- 1.3.1
natural :: Integer -> Integer
natural n | n <= 1 = 0
          | n > 1 = 1 + natural (n-1)

-- 1.3.2
odds :: Integer -> Integer
odds n  | n <= 1 = 0
        | even n = 0
        | odd n = 1 + odds (n-2)

-- 1.3.3
sumRec :: Int -> Int
sumRec n | n <= 0 = 0
         | n > 0 = n + sumRec (n-1)

-- 1.3.4
fact :: Integer -> Integer
fact n  | n == 0 = 1
        | n > 0 = n * fact (n-1)
        | n < 0 = n * fact (n+1)

-- 1.3.5
sumFact :: Integer -> Integer
sumFact n | n == 0 = fact 0
          | otherwise = (fact n) + sumFact (n-1)

-- 1.3.6
-- n-th element = a + (n-1)d
arithmeticSeries :: Integer -> Integer -> Integer -> Integer
arithmeticSeries a n d  | n < 0 = a    -- error prevention
                        | n == 1 = a
                        | n > 1 = d + arithmeticSeries a (n-1) d

-- 1.3.7
natural' :: Integer -> Integer
natural' n = arithmeticSeries 1 n 1

odds' :: Integer -> Integer
odds' n = arithmeticSeries 1 n 2

-- 1.3.8
arithmeticSumRec :: Double -> Double -> Double -> Double
arithmeticSumRec a n d  | n <= 0 = 0
                        | n == 1 = a
                        | n > 1 = a + (n-1)*d + arithmeticSumRec a (n-1) d

-- 1.3.9
multRec :: Int -> Int -> Int
multRec m n | m == 0 || n == 0 = 0
            | n > 0 = m + multRec m (n-1)
            | n < 0 = m + multRec m (n+1)

-- 1.3.10
rangeProduct :: Int -> Int -> Int
rangeProduct m n | n < m = 1
                 | otherwise = m * rangeProduct (m+1) n
                 -- | n * rangeProduct m (n-1)  -- alternative

-- 1.3.11
intSqrt :: Int -> Int
intSqrt n  = intSqrt' 0 n
    where
        intSqrt' :: Int -> Int -> Int       -- can be left out
        intSqrt' root num | root^2 == num = root
                          | root^2 < num && (root+1)^2 > num = root
                          | otherwise = intSqrt' (root+1) num

-- 1.3.12 - varja

---------------
-- EXERCISE 1.4
---------------
-- 1.4.1
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- 1.4.2
fiboTwo :: Int -> (Int, Int)
fiboTwo 0 = (0, 1)
fiboTwo n = step $ fiboTwo (n-1) where
    step (x, y) = (y, x+y)

superFibo :: Int -> Int
superFibo n = fst twoPreviousFibo + snd twoPreviousFibo
    where twoPreviousFibo = fiboTwo (n-2)

------------------------------------------------------------------
-- @ operator usage: 
-- lst @ (h:tl)
-- giving the the list, its head and its tail a name
-- by pattern matching
------------------------------------------------------------------

myFunc n m = n ++ m

myFuncAdd :: Num a => a -> a -> a
myFuncAdd n m = n + m

myFuncDiv :: (Fractional a) => a -> Integer -> a
myFuncDiv n m = n + m
-- 2 `div` 3

-- func :: MyTypeClass a => a -> a
--  + / * ++
arit
