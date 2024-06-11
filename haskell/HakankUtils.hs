


module HakankUtils where

import Data.Char
import Data.List    


-- ensure that xs is increasing
increasing :: Ord a => [a] -> Bool
increasing xs = xs == sort xs

-- decreasing :: Ord a => [a] -> Bool
-- decreasing xs = xs == sortBy (>=) xs

-- Count the number of i in xs
count :: Int -> [Int] -> Int
count i xs = sum [1 | x <- xs, x == i]

-- Ensure that the list contains unique values
allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = all (\y -> x /= y) xs && allDifferent xs

-- Another and simpler approach
allDifferent2 :: Eq a => [a] -> Bool
allDifferent2 xs
   | null [] = True
   | otherwise = nub xs == xs

allDifferent3 :: Eq a => [a] -> Bool
allDifferent3 x = x == nub x
    
-- > liftM2 (*) [1..4] [5..8]
-- [5,6,7,8,10,12,14,16,15,18,21,24,20,24,28,32]
-- > chunksOf 3 $ liftM2 (*) [1..4] [5..8]
-- [[5,6,7],[8,10,12],[14,16,15],[18,21,24],[20,24,28]]
chunksOf n xs | length xs < n = []
chunksOf n xs | length xs >= n = take n xs : chunksOf n (drop n xs)

--
-- runningChunksOf n xs
-- Create a list of each 'running sublist' of length n in xs
--  > runningChunksOf 3 [1..9]
--  [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9]]
-- 
runningChunksOf :: Int -> [a] -> [[a]]
runningChunksOf n xs
  | null xs   = []
  | length xs < n = []
  | otherwise = take n xs : runningChunksOf n (tail xs)

                                 
-- Fibonacci
fiblist x y = x : fiblist y (x+y)
fib n = fiblist 0 1 !! n

fiblist2 n = take n $ fibs
             where
               fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

isqrt = floor . sqrt . fromIntegral
                      

--
-- Primes
--
primes = 2 : filter (null . tail . primeFactors) [3,5..]
primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps


-- Wrapped in a single function, otherwise Curry complained about
-- possible non-deterministic due to multiple clauses...
nextPrime n = if n <= 1 then
                2
              else if n == 2 then
                3
              else if n == 4 || n == 3 then
                5
              else 
                nextPrime2 (n + 1)

nextPrime2 n = if isPrime n then n else nextPrime2 (n + 1)


toInt2 :: Float -> Int
toInt2 = round

isPrime :: Int -> Bool
isPrime n = if n < 2 then
               False
            else if n == 2 || n == 3 then
               True
            else if n `mod` 2 == 0 then
               False
            else
               if and [n `mod` i > 0 | i <- [3,5..m]] then
                 True
               else
                 False
               where
                 m = toInt2 $ sqrt $ fromIntegral n

-- I asked ChatGPT-4 about an efficient Haskell implementation.
-- This is much slower than my isPrime.
-- Here's euler7b with isPrime2:
--   > euler7b
--   104743
--   Execution time: 22046 msec. / elapsed: 24698 msec.
-- vs (using isPrime)
--   Execution time: 14658 msec. / elapsed: 16320 msec.
--
isPrime2 :: Int -> Bool
isPrime2 n | n < 2 = False
          | otherwise = null [ x | x <- [2..isqrt n], n `mod` x  == 0]
  where
    isqrt = floor . sqrt . fromIntegral


primesN n = [i | i <- [2] ++ [3,5..n], isPrime i]
primesN2 n = [i | i <- [2] ++ [3,5..n], isPrime2 i]

--
-- Number of divisors of a number n
-- Based on prime factors of n:
--  product of the 1+(number of occurrences of each prime factor)
--
numDivisors :: Int -> Int
numDivisors n = product [e+1 | (_,e) <- collect $ primeFactors n]

--
-- Sum of proper divisors. Slow.
--
sumDivisorsSlow :: Int -> Int
sumDivisorsSlow n = sum $ properDivisors n

-- Sum of proper divisors. Faster version
--
-- This is aort of my Picat/Prolog version sum_divisors3/4
--
sumDivisors :: Int -> Int
sumDivisors n = sumDivisors_ 2 n 1

sumDivisors_ i n sum0
      | i > isqrt n    = sum0                      -- too large
      | n `mod` i == 0 = sumDivisors_ (i+1) n sum2 -- i is a divisor of n
      | n `mod` i  > 0 = sumDivisors_ (i+1) n sum0  -- i is not a divisor of n
      where
      sum1 = sum0 + i
      sum2 = if i /= n `div` i then sum1 + n `div` i  else sum1

--
-- All proper divisors of n (i.e. exceluding n)
--
properDivisors :: Int -> [Int]
properDivisors n = [i | i <- [1..n `div` 2], n `mod` i == 0]
-- properDivisors n = [i | i <- [1..isqrt n], n `mod` i == 0]


               
--
-- Factorial
--
fact n = product [1..n]

--
-- Convert a number to digits
--
numToDigits n = map (digitToInt) $ show n

--
-- Convert a list of digits to number
--
digitsToNum xs = sum [d*v | (d,v) <- zip (map (\i -> 10^(i-1)) $ reverse [1..length xs]) xs]

--
-- Convert a string of digits to a number
--
strToNum :: [Char] -> Int
strToNum s = digitsToNum $ map digitToInt s
                 
--
-- Matrices
--

-- Access the (i,j)'th element of a matrix (list of lists)
-- Note: Here's no checking that the i'th list or the j'th element exists.
matrixElement m i j = m !! i !! j


--
-- Running product/sum/fun
--

--
-- Generalized version of running<Fun>
--
runningFun f n [] = []
runningFun f n (x:xs) = (f $ take n (x:xs)) : runningFun f n xs

-- Running prod over a list
-- runningProd n [] = [] 
-- runningProd n (x:xs) = (product $ take n (x:xs)) : runningProd n xs
runningProd n xs = runningFun product n xs
runningSum n xs = runningFun sum n xs



--
-- Count the number of occurrences of each element in the list (x:xs)
-- Assumption: The list shoule be sorted.
-- Example:
--   > collect [2,2,3,3,5,5,5,7,11,13,17]
--   [(2,2),(3,2),(5,3),(7,1),(11,1),(13,1),(1,17)]
-- (I would rather use a hash table here, but it seems to be much messier to handle).
-- TODO: Try to implement a hash table version!
collect [] = []
collect [x] = [(x,1)]
collect (x:xs) | xs /= [] = (x,length t) : (collect (dropWhile (==x) xs))
                 where
                   t = takeWhile (==x) (x:xs)


--
-- Count the number of occurrences of each element in the list (x:xs)
-- The result list is [(count,a),(count,b)..], i.e. the pairs are reversed
-- compared to collect
-- Assumption: The list shoule be sorted.
-- Example:
--   > collect2 [2,2,3,3,5,5,5,7,11,13,17]
--   [(2,2),(2,3),(3,5),(1,7),(1,11),(1,13),(1,17)]
collect2 :: Eq a => [a] -> [(Int,a)]
collect2 [] = []
collect2 [x] = [(1,x)]
collect2 (x:xs) | xs /= [] = (length t,x) : (collect2 (dropWhile (==x) xs))
                 where
                   t = takeWhile (==x) (x:xs)

                       
--
-- Day of week, Sakamoto's method
-- http:%en.wikipedia.org/wiki/Weekday_determination#Sakamoto.27s_Method
-- 
-- Note: month is 0 based
dow y m d = dow
     where
     t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
     yy = if m < 3 then y - 1 else y
     dow = (yy + yy `div` 4 - yy `div` 100 + yy `div` 400 + (t!!m) + d) `mod` 7

-- Note: month is 0 based!
monthDays year month = days !! month
                  where
                       feb = if leapYear year then 29 else 28
                       days = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

leapYear year = if (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0 then True else False



--
-- rotateLeft n xs:
-- Returns xs returned n steps to the right.
--
rotateRight :: Int -> [Int] -> [Int]
rotateRight n xs = (drop n xs) ++ (take n xs)

rotate :: Int -> [Int] -> [Int]
rotate n xs = rotateRight n xs

--
-- rotateLeft n xs:
-- Returns xs returned n steps to the left.
-- TODO: Make this faster...
--
rotateLeft :: Int -> [Int] -> [Int]
rotateLeft n xs = reverse $ rotateRight n (reverse xs)



-- palin :: [a] -> Bool
palin :: Eq a => [a] -> Bool
palin xs = xs == reverse xs

decToBase2 :: Int -> Int -> [Int]
decToBase2 n base
     | n > 0     = (decToBase2 d base) ++ [m]
     | otherwise = []
     where
        m = n `mod` base
        d = n `div` base

-- This seems to be a little faster
decToBase' :: Int -> Int -> [Int]
decToBase' n base
     | n > 0     = m : decToBase' d base
     | otherwise = []
     where
        m = n `mod` base
        d = n `div` base

decToBase :: Int -> Int -> [Int]
decToBase n base = reverse $ decToBase' n base


--
-- Length of a decimal number
--
nlen :: Int -> Int
nlen n =1+floor(logBase 10 (fromIntegral n))


--
-- Split a string with separator character c
--
splitSep sep s = words [if c == sep then ' ' else c | c <- s]

--
-- Delete all occurrences of character c from string a
--
deleteAll c a@(x:xs)
  | a == [] || a == [c]   = []
  | otherwise = if x == c then deleteAll c xs else x : deleteAll c xs


--
-- range xs from to
-- return the sublist of xs from index from to to
--
range :: [a] -> Int -> Int -> [a]
range xs from to = take (to-from+1) $ drop from  xs
