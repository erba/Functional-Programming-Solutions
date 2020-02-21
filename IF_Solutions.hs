import Data.Char    -- für digitToInt
import Data.List

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
-- EXERCISE SHEET 1
-------------------
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

-- 1.2.1
-- func :: [a] -> Int
--
-- 1.2.2
-- func :: Char -> Char
--
-- 1.2.3
-- func :: String -> String -> Bool    works but
-- func :: [a] -> [a] -> Bool     is the better solution

-- 1.3.1
natural :: Int -> Int
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
fact :: Int -> Int
fact n  | n == 0 = 1
        | n > 0 = n * fact (n-1)
        | n < 0 = n * fact (n+1)

-- 1.3.5
sumFact :: Int -> Int
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
-------------------
-- EXERCISE SHEET 2
-------------------
-- 2.1.1
listEvens :: Int -> Int -> [Int]
listEvens x y | x > y = []
listEvens x y | otherwise = [k | k <- [x, (x+1) ..  y], even k]


-- 2.1.2
type Triple = (Int, Int, Int)
--
pythagoreanTriples :: Int -> [Triple]
pythagoreanTriples n | n <= 0 = []      -- error prevention
                     | otherwise = [(a, b, c) | a <- smallerEqualN, b <- smallerEqualN, c <- smallerEqualN, a^2 + b^2 == c^2]
                         where smallerEqualN = [k | k<-[0 .. n]]

-- 2.1.3
addPairwise :: [Int] -> [Int] -> [Int]
addPairwise a b = zipWith (+) a b

-- 2.1.4
subList :: [a] -> (Int, Int) -> [a]
subList [] _       = []
subList lst (i, j) = [lst!!index | index <- [i .. j]]
--
subList' :: [a] -> (Int, Int) -> [a]
subList' [] _       = []
subList' lst (i, j) = drop i $ take (j+1) lst

-- 2.1.5
together :: [a] -> [(a,a)]
together []     = []
together (x:[]) = []
together lst    = [ ( lst!!index, lst!!(index+1) ) | index <- [0 .. lastIndex-1] ]
                where lastIndex = (length lst) - 1

-- 2.2.1
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (head:tail) element = element == head || contains tail element
--
contains' :: Eq a => [a] -> a -> Bool
contains' lst element = any (== element) lst

-- 2.2.2
nth :: Int -> [a] -> Maybe a
nth out lst   | out < 0 || out > ( (length lst) - 1) = Nothing    -- not really necessary, since thiese csaes are handled by the following lines; but for the sake of efficiency
nth _ []      = Nothing
nth 0 (x: xs) = Just x
nth n (x: xs) = nth (n-1) xs

-- 2.2.3
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove i (x:xs) | i == x = remove i xs
                | i /= x = x : (remove i xs)
--
remove' :: Eq a => a -> [a] -> [a]
remove' i lst = filter (/= i) lst

-- 2.2.4
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute a b (x:xs) | x == a = b : (substitute a b xs)
                      | x /= a = x : (substitute a b xs)
--
substitute' :: Eq a => a -> a -> [a] -> [a]
substitute' a b lst = map (aToB) lst
    where aToB x | x == a = b
                 | otherwise = x

-- 2.3.1
addMinutes :: [(Int, Int)] -> (Int, Int)
addMinutes [] = (0, 0)
addMinutes (h:tail) = addTwoTimes h (addMinutes tail)
    where addTwoTimes (m1,s1) (m2,s2) = (m1 + m2 + (s1 + s2) `div` 60, (s1+s2) `mod` 60)

-- 2.3.2
type Moves = (Char, Char)
type Solution = [Moves]
hanoi :: Int -> Solution
hanoi n = hanoi' n 'a' 'c'
--
hanoi' :: Int -> Char -> Char -> Solution
hanoi' 0 _ _ = []    -- error prevention
hanoi' _ startPole endPole | startPole == endPole = []
hanoi' _ startPole _ | startPole /= 'a' && startPole /= 'b' && startPole /= 'c' = [('E', 'R')]    -- error
hanoi' _ _ endPole | endPole /= 'a' && endPole /= 'b' && endPole /= 'c' = [('E', 'R')]    -- error
--
hanoi' 1 startPole endPole = [ (startPole, endPole) ]
--
hanoi' 2 startPole endPole = hanoi' 1 startPole freePole ++ hanoi' 1 startPole endPole ++ hanoi' 1 freePole endPole
    where freePole = getFreePole startPole endPole
--
hanoi' 3 startPole endPole = hanoi' 2 startPole freePole ++ hanoi' 1 startPole endPole ++ hanoi' 2 freePole endPole
    where freePole = getFreePole startPole endPole
--
hanoi' n startPole endPole = hanoi' (n-1) startPole freePole ++ hanoi' 1 startPole endPole ++ hanoi' (n-1) freePole endPole
    where freePole = getFreePole startPole endPole
--
getFreePole :: Char -> Char -> Char
getFreePole 'a' 'b' = 'c'
getFreePole 'a' 'c' = 'b'
getFreePole 'b' 'a' = 'c'
getFreePole 'b' 'c' = 'a'
getFreePole 'c' 'a' = 'b'
getFreePole 'c' 'b' = 'a'

-- 2.4.1
addDashes :: [String] -> [String]
addDashes lst = map (\x -> "-- " ++ x) lst

-- 2.4.2
swapPairs :: [(a,b)] -> [(b, a)]
swapPairs lst = map (\(x, y) -> (y, x)) lst

-- 2.4.3
applyEach :: [((a -> b), a)] -> [b]
applyEach lst = map (\(f, x) -> f x) lst
--
applyEach' :: [((a -> b), a)] -> [b]
applyEach' lst = [f y | (f,y) <- lst]

-- 2.4.4
theSame :: String -> Bool
theSame "" = True
theSame str@(x:_) = foldr1 (&&) (map ( == x) str)

-- 2.5.1
twice :: (a -> a) -> a -> a
twice f x = (f . f) x
-- twice f x = f (f x)
-- twice f x = f $ f x

-- 2.5.2
maxRef :: (Int -> Int) -> Int -> Int
maxRef f n | n < 0 = 0
           | n >= 0 =  max (f n) (maxRef f (n - 1))

-- 2.5.3
oneZero :: (Int -> Int) -> Int -> Bool
oneZero f n = any (== 0) [f x | x <- [0 .. n]]

-- 2.5.4
iter :: Int -> (a -> a) -> a -> a
iter 1 f x = f x
iter n f x = f $ iter (n-1) f x
--
iter' :: Int -> (a -> a) -> a -> a
iter' n f x = foldr ($) (f x) (replicate (n-1) f)

-- 2.5.5
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (h:tail) = f h || myAny f tail
--
myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f lst = foldr (||) False [f x | x <- lst]
----
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (h:tail) = f h && myAll f tail
--
myAll' :: (a -> Bool) -> [a] -> Bool
myAll' f lst = foldr (&&) True [f x | x <- lst]
----
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (h:tail) = h + mySum tail
--
mySum' :: Num a => [a] -> a
mySum' lst = foldr (+) 0 lst

-- 2.5.6
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip lst = foldr (\x y -> (fst x : fst y, snd x : snd y)) ([], []) lst

-- 2.5.7
myMap :: (a -> b) -> [a] -> [b]
myMap f lst = [f k | k <- lst]

-- 2.5.8
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (h:tail) = if f h == True
                         then h : myFilter f tail
                      else myFilter f tail

-- 2.5.9
natInd :: a -> (Int -> a -> a) -> Int -> a
natInd def f 0 = def
-- natInd def f n = f (natInd def f (n-1))
-- X0 = 0
-- Xn+1 = Xn + 1
-- Xn+1 = step_function(Xn)
-------------------
-- EXERCISE SHEET 3
-------------------

-- 3.1.1
data Season = Spring | Summer | Autumn | Winter
    deriving (Eq, Show, Enum)
theSeasons :: [Season]
theSeasons = enumFrom Spring

-- 3.1.2
seasonsFrom :: Season -> [Season]
seasonsFrom n = enumFrom n
--
seasonsFrom' :: Season -> [Season]
seasonsFrom' n = [n .. Winter]

-- 3.1.3
mapSeasonsFrom :: [Season] -> [ [Season] ]
mapSeasonsFrom n = [seasonsFrom k | k <- n]

-- 3.1.4
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Eq, Ord, Show, Enum)
-- fromEnum Feb
-- toEnum 2::Motth

-- 3.1.5
monthFromTo :: Month -> Month -> [Month]
monthFromTo f t = [f .. t]

-- 3.1.6
monthToSeason :: Month -> Season
monthToSeason n | elem n ([Dec] ++ [Jan .. Feb]) = Winter
                | elem n [Mar .. May] = Spring
                | elem n [Jun .. Aug] = Summer
                | elem n [Sep .. Nov] = Autumn
-- monthToSeason Dec = Winter
-- monthToSeason Jan = Winter
-- monthToSeason Feb = Winter
-- monthToSeason Mar = Spring
-- monthToSeason Apr = Spring
-- monthToSeason May = Spring
-- monthToSeason Jun = Summer
-- monthToSeason Jul = Summer
-- monthToSeason Aug = Summer
-- monthToSeason Sep = Autumn
-- monthToSeason Oct = Autumn
-- monthToSeason Nov = Autumn

-- 3.1.7
data MyBoolean = MyFalse | MyTrue
    deriving Show
boolToMyBoolean :: Bool -> MyBoolean
boolToMyBoolean True = MyTrue
boolToMyBoolean False = MyFalse
--
myBooleanToBool :: MyBoolean -> Bool
myBooleanToBool MyFalse = False
myBooleanToBool MyTrue = True

-- 3.1.8
(&:&) :: MyBoolean -> MyBoolean -> MyBoolean
(&:&) MyTrue MyTrue = MyTrue
(&:&) _ _ = MyFalse
--
(|:|) :: MyBoolean -> MyBoolean -> MyBoolean
(|:|) MyTrue _ = MyTrue
(|:|) _ MyTrue = MyTrue
(|:|) _ _ = MyFalse

-- 3.1.9
myOr :: [MyBoolean] -> MyBoolean
myOr [] = MyFalse
myOr (h:tail) = h |:| myOr tail
--
myAnd :: [MyBoolean] -> MyBoolean
myAnd [] = MyTrue
myAnd (h:tail) = h &:& myAnd tail
--
myOr' :: [MyBoolean] -> MyBoolean
myOr' lst = boolToMyBoolean $ or $ map (myBooleanToBool) lst
--
myAnd' :: [MyBoolean] -> MyBoolean
myAnd' lst = boolToMyBoolean $ and $ map (myBooleanToBool) lst
--
myOr'' :: [MyBoolean] -> MyBoolean
myOr'' lst = foldr (|:|) MyFalse lst
--
myAnd'' :: [MyBoolean] -> MyBoolean
myAnd'' lst = foldr (&:&) MyTrue lst

-- 3.2.1
data Bit = I | O | Bit Bit deriving (Eq, Show)
bitsToInt :: [Bit] -> Int
bitsToInt lst = bitsToIntRec lst 0
    where bitsToIntRec :: [Bit] -> Int -> Int
          bitsToIntRec ls index | index == length ls - 1 = if ls!!index == I then 2^(length ls - 1 - index) else 0
          bitsToIntRec ls index | ls!!index == I = 2^(length ls - 1 - index) + bitsToIntRec ls (index + 1)
          bitsToIntRec ls index | ls!!index == O = bitsToIntRec ls (index + 1)
--
bitsToInt' :: [Bit] -> Int
bitsToInt' [] = 0
bitsToInt' lst@(h:t) | h == I = 2^(length lst - 1) + bitsToInt' t
                     | h == O = bitsToInt' t
--
bitsToInt'' :: [Bit] -> Int
bitsToInt'' lst = snd (foldr (sumIOs) (O, 0) (zip [bit | bit <- lst] [power | power <- [length lst - 1, length lst - 2 .. 0]] ))
    where sumIOs :: (Bit, Int) -> (Bit, Int) -> (Bit, Int)
          sumIOs (bit1, power1) (_, sum) = (O, bitTo01 bit1 * 2^power1 + sum)
          bitTo01 O = 0
          bitTo01 I = 1

-- 3.2.2
data Number = Exact Int | Approx Float
rounded :: Number -> Int
rounded (Exact x) = x
rounded (Approx x) = round x

-- 3.4.1
data Age = Years Int deriving Show
data Name = Name String String deriving Show
data Person = Person Name Age deriving Show
howOld :: Person -> Age
howOld (Person _ age) = age
--
addAges :: Person -> Person -> Age
addAges (Person _ (Years x)) (Person _ (Years y)) = Years (x+y)

-- 3.4.2
-- -- to run this paragraph comment out the next two
-- data Expr = Lit Int | Add Expr Expr | Sub Expr Expr
-- size :: Expr -> Int
-- size (Lit _) = 0
-- size (Add e1 e2) = 1 + (size e1) + (size e2)
-- size (Sub e1 e2) = 1 + (size e1) + (size e2)

-- 3.4.3
-- -- to run this paragraph comment out the next and previous one
-- data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
-- --
-- eval :: Expr -> Int
-- eval (Lit n) = n
-- eval (Add e1 e2) = (eval e1) + (eval e2)
-- eval (Sub e1 e2) = (eval e1) - (eval e2)
-- eval (Mul e1 e2) = (eval e1) * (eval e2)
-- eval (Div e1 e2) = (eval e1) `div` (eval e2)
-- --
-- size :: Expr -> Int
-- size (Lit _) = 0
-- size (Add e1 e2) = 1 + (size e1) + (size e2)
-- size (Sub e1 e2) = 1 + (size e1) + (size e2)
-- size (Mul e1 e2) = 1 + (size e1) + (size e2)
-- size (Div e1 e2) = 1 + (size e1) + (size e2)

-- 3.5.1
-- to run this paragraph comment out the previous two
data Expr = Lit Int | OpExp Op Expr Expr deriving Eq
data Op = Add | Sub | Mul | Div | Mod deriving Eq
eval :: Expr -> Int
eval (Lit n) = n
eval (OpExp Add e1 e2) = (eval e1) + (eval e2)
eval (OpExp Sub e1 e2) = (eval e1) - (eval e2)
eval (OpExp Mul e1 e2) = (eval e1) * (eval e2)
eval (OpExp Div e1 e2) = (eval e1) `div` (eval e2)
eval (OpExp Mod e1 e2) = (eval e1) `mod` (eval e2)
--
size :: Expr -> Int
size (Lit _) = 0
size (OpExp Add e1 e2) = 1 + (size e1) + (size e2)
size (OpExp Sub e1 e2) = 1 + (size e1) + (size e2)
size (OpExp Mul e1 e2) = 1 + (size e1) + (size e2)
size (OpExp Div e1 e2) = 1 + (size e1) + (size e2)
size (OpExp Mod e1 e2) = 1 + (size e1) + (size e2)

-- 3.5.2
data IExpr = ILit Int | IADD IExpr IExpr | ISUB IExpr IExpr | IMUL IExpr IExpr | IMOD IExpr IExpr | IF BExpr IExpr IExpr
data BExpr = BLit Bool | AND BExpr BExpr | OR BExpr BExpr | NOT BExpr | EQUAL IExpr IExpr
iEval :: IExpr -> Int
iEval (ILit x) = x
iEval (IADD e1 e2) = (iEval e1) + (iEval e2)
iEval (ISUB e1 e2) = (iEval e1) - (iEval e2)
iEval (IMUL e1 e2) = (iEval e1) * (iEval e2)
iEval (IMOD e1 e2) = (iEval e1) `mod` (iEval e2)
iEval (IF be e1 e2) | bEval be == True = iEval e1
                    | bEval be == False = iEval e2
--
bEval :: BExpr -> Bool
bEval (BLit b) = b
bEval (AND e1 e2) = (bEval e1) && (bEval e2)
bEval (OR e1 e2)  = (bEval e1) || (bEval e2)
bEval (NOT e1)    = not $ bEval e1
bEval (EQUAL e1 e2)  = (iEval e1) == (iEval e2)

-- 3.6.1
data Pair a = Pair a a
    deriving Show    -- derive from Show to print to console
swapPair :: Pair a -> Pair a
swapPair (Pair a b) = Pair b a
--
eqPair :: Eq a => Pair a -> Bool
eqPair (Pair a b) = (a == b)

-- 3.6.2
data List a = EmptyList | Cons a (List a)
    deriving (Eq, Ord, Show, Read)
isEmpty :: List a -> Bool
isEmpty EmptyList = True
isEmpty _ = False
--
lengthOfList :: List a -> Int
lengthOfList EmptyList = 0
lengthOfList (Cons a list) = 1 + lengthOfList list

-- 3.7.1
data Tree a = Empty | Node a (Tree a) (Tree a) | Leaf a
    deriving Show
howMany :: Tree a -> Int
howMany Empty                    = 0
howMany (Leaf a)                 = 1
howMany (Node a branchL branchR) = 1 + (howMany branchL) + (howMany branchR)

-- 3.7.2
depth :: Tree a -> Int
depth Empty                    = 0
depth (Leaf a)                 = 1
depth (Node a branchL branchR) = max (1 + depth branchL) (1 + depth branchR)
-- shortest path in a tree???

-- 3.7.3
reflect :: Tree a -> Tree a
reflect Empty                    = Empty
reflect (Leaf a)                 = Leaf a
reflect (Node a branchL branchR) = Node a (reflect branchR) (reflect branchL)
-- for testing: ( Node 'a' (Node 'b' (Node 'd' (Leaf 'h') (Leaf 'i')) (Node 'e' (Leaf 'j') (Leaf 'k'))) (Node 'c' (Node 'f' (Leaf 'l') (Leaf 'm')) (Node 'g' (Leaf 'n') (Leaf 'o'))) )

-- 3.7.4
traversal :: Tree a -> [a]
traversal Empty        = []
traversal (Leaf x)     = [x]
traversal (Node x l r) = traversal l ++ [x] ++ traversal r
--
post :: Tree a -> [a]
post Empty                    = []
post (Leaf x)                 = [x]
post (Node x branchL branchR) = post branchL ++ post branchR ++ [x]
--
pre :: Tree a -> [a]
pre Empty                    = []
pre (Leaf x)                 = [x]
pre (Node x branchL branchR) = [x] ++ pre branchL ++ pre branchR

-- 3.7.5
normalise :: Tree a -> Tree a
normalise Empty                    = Empty
normalise (Leaf x)                 = Leaf x
normalise (Node x Empty Empty)     = Leaf x
normalise (Node x branchL branchR) = Node x (normalise branchL) (normalise branchR)

-- 3.7.6
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty                    = Empty
mapTree f (Leaf x)                 = Leaf (f x)
mapTree f (Node x branchL branchR) = Node (f x) (mapTree f branchL) (mapTree f branchR)
--
natural'' :: Int -> Bool    -- already natural and natural' defined above
natural'' n = (n >= 0)
--
plusOne :: Int -> Int
plusOne x = x + 1

-- 3.7.7
-- traversal (Tree a)  ->  pre (Tree a) -> Tree a
listsToTree :: Eq a => [a] -> [a] -> Tree a
listsToTree [] [] = Empty
listsToTree (x:[]) (x':[]) = Leaf x    -- Tree is only one Leaf (x == y)
listsToTree travList@(th:tt) preList@(ph:pt) = Node treeRoot (listsToTree travLeft preLeft) (listsToTree travRight preRight)
    where
        treeRoot = preList!!0                                                          -- root is first element of preList
        travLeft = take (maybeToIndex $ elemIndex treeRoot travList) travList          -- travLeft = all before root in travList
        travRight = drop ((+1) $ maybeToIndex $ elemIndex treeRoot travList) travList  -- travRight is all after root in travList
        preLeft = take (length travLeft) (drop 1 preList)                              -- preLeft = drop treeRoot(first element) + take as many elements as travLeft has
        preRight = drop (length travLeft + 1) preList                                  -- drop treeRoot(firs element) + as many elements as travLeft has
        maybeToIndex Nothing = -1
        maybeToIndex (Just x) = x
-- for testing:
fullBinaryTreeDepth4 = (Node 'a' (Node 'b' (Node 'd' (Node 'h' (Leaf 'p') (Leaf 'q')) (Node 'i' (Leaf 'r') (Leaf 's'))) (Node 'e' (Node 'j' (Leaf 't') (Leaf 'u')) (Node 'k' (Leaf 'v') (Leaf 'w')))) (Node 'c' (Node 'f' (Node 'l' (Leaf 'x') (Leaf 'y')) (Node 'm' (Leaf 'z') (Leaf '1'))) (Node 'g' (Node 'n' (Leaf '2') (Leaf '3')) (Node 'o' (Leaf '4') (Leaf '5')))))
derived_fullBinaryTreeDepth4 = listsToTree (traversal fullBinaryTreeDepth4) (pre fullBinaryTreeDepth4)
--
someUnevenBinaryTree = Node 'a' (Node 'b' (Node 'c' (Node 'd' (Leaf 'e') Empty) Empty) Empty) Empty
derived_someUnevenBinaryTree = listsToTree (traversal someUnevenBinaryTree) (pre someUnevenBinaryTree)
--
someLongBinaryTree = Node 'a' (Node 'b' (Node 'c' (Node 'e' (Node 'g' (Node 'j' (Node 'l' (Leaf 'm') Empty) Empty) (Leaf 'k')) Empty) (Node 'f' (Leaf 'h') (Leaf 'i'))) (Leaf 'd')) Empty
derived_someLongBinaryTree = listsToTree (traversal someLongBinaryTree) (pre someLongBinaryTree)

-- 3.8.1
data GenericTree a = GTEmpty | GTLeaf a | GTNode a [GenericTree a] deriving Show    -- Show for console printing

-- 3.8.2
countGTLeaves :: GenericTree a -> Integer
countGTLeaves GTEmpty = 0
countGTLeaves (GTLeaf a) = 1
countGTLeaves (GTNode a branchList) = sum (map countGTLeaves branchList)
-- for testing: GTNode 'a' [ (GTNode 'b' [GTNode 'd' [GTNode 'h' [GTNode 'e' [GTLeaf 'n', GTLeaf 'o'], GTLeaf 'm'], GTLeaf 'i'] , GTLeaf 'e']) , GTNode 'c' [GTNode 'f' [GTLeaf 'j'], GTNode 'g' [GTLeaf 'k']]]

-----------------
-- Personal notes
-----------------
-- Polymorphic functions must either have a type class(Num a=>...) in the signature or leave the signature out all-together and let Haskell infer it
-- Which operators are defined by Num, Ord, Eq, Show, Read???
-- $ opens a bracket and closes it as far right as possible
-- "Foldable t => t a" similar to IList<a> in C#/Java
-- difference between "zip list1 list2" and "[(l1,l2) | l1 <- list1, l2 <- list2]" ?
-- [0..3] = [0,1,2,3]    [3..0] = []    [3,2..0] = [3,2,1,0]
