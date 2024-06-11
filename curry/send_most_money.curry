{- 
  
  SEND+MOST=MONEY in Curry
  
  Solve the alphametic equation SEND+MOST=MONEY and maximize MONEY.

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


send_most_money' :: [[Int]]
send_most_money' = let
                [s,e,n,d,m,o,t,y] = take 8 (domain 0 9)
                x = [s,e,n,d,m,o,t,y] 
                send = head (domain 1000 9999)
                most = head (domain 1000 9999)                
                money = head (domain 10000 99999)
             in
                solveFDAll [FirstFail,Bisect] [money,send,most] $
                -- solveFDAll [FirstFail,Bisect,Maximize money] [money,send,most] $ -- does not work
                allDifferent x /\
                s ># 0 /\ m ># 0 /\
                send =# 1000*s + 100*e + 10*n + d /\
                most =# 1000*m + 100*o + 10*s + t /\
                money =# 10000*m + 1000*o + 100*n + 10*e + y /\
                money =# send + most

send_most_money :: (Int, Int, Int)
send_most_money = head [(send,most,money) | let [money,send,most] = maximumBy (\a b -> compare (head a) (head b)) $ send_most_money']

maxVal :: [[Int]] -> [Int]
maxVal xs = maximumBy (\a b -> compare (head a) (head b)) xs

--
-- Get all optimal solutions
--
send_most_money_b :: [(Int, Int, Int)]
send_most_money_b = [(send,most,money)  | [money,send,most] <- smm,  money == m]
                    where
                       smm = send_most_money'
                       m = head $ maxVal smm

--
-- Using solveFDOne + Maximize (+ non-det)
-- Slower
send_most_money2 = let
                [s,e,n,d,m,o,t,y] = take 8 (domain 0 9)
                x = [s,e,n,d,m,o,t,y] 
                send = head (domain 1000 9999)
                most = head (domain 1000 9999)                
                money = head (domain 10000 99999)
                moneyOpt = anyOf [10000..99999]
             in
                solveFDOne [FirstFail,Bisect,Maximize moneyOpt] [send,most,money] $
                fd moneyOpt =# money /\
                allDifferent x /\
                s ># 0 /\ m ># 0 /\
                send =# 1000*s + 100*e + 10*n + d /\
                most =# 1000*m + 100*o + 10*s + t /\
                money =# 10000*m + 1000*o + 100*n + 10*e + y /\
                money =# send + most


{-
  One optimal solution
  (9782,1094,10876)
  Execution time: 39 msec. / elapsed: 43 msec.

-}
main :: IO ()
main = do
          print send_most_money


{-
  All optimal solutions
  [(9784,1092,10876),(9782,1094,10876)]
  Execution time: 31 msec. / elapsed: 31 msec.
-}
main2 :: IO ()
main2 = do
          print send_most_money_b

{-
  Using solveFDOne and Minimize (+ non-det)
  Slower
  [9342,1095,10437]
  Execution time: 98 msec. / elapsed: 99 msec.

-}
send_most_money2 :: [Int]
main3 = send_most_money2
