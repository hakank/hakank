{- 
  
  SEND+MORE=MONEY in Curry
  
  Solve the alphametic equation SEND+MORE=MONEY

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD


send_more_money :: [Int]
send_more_money = let
                    [s,e,n,d,m,o,r,y] = take 8 (domain 0 9)
                    x = [s,e,n,d,m,o,r,y]
                  in
                    solveFD [FirstFail,Bisect] x $
                    allDifferent x /\
                    s ># 0 /\ m ># 0 /\
                    1000*s + 100*e + 10*n + d +
                    1000*m + 100*o + 10*r + e =#
                    10000*m + 1000*o + 100*n + 10*e + y

--
-- A little fancier
--
send_more_money2 :: [Int]
send_more_money2 = let
                [s,e,n,d,m,o,r,y] = take 8 (domain 0 9)
                x = [s,e,n,d,m,o,r,y] 
                send = head (domain 1000 9999)
                more = head (domain 1000 9999)                
                money = head (domain 10000 99999)
             in
                solveFD [FirstFail,Bisect] [send,more,money] $
                allDifferent x /\
                s ># 0 /\ m ># 0 /\
                send =# 1000*s + 100*e + 10*n + d /\
                more =# 1000*m + 100*o + 10*r + e /\
                money =# 10000*m + 1000*o + 100*n + 10*e + y /\
                money =# send + more

{-
  [9,5,6,7,1,0,8,2]
  Execution time: 25 msec. / elapsed: 25 msec.

-}
main :: [Int]
main = send_more_money


{-
  "9567+ 1085 = 10652"
  Execution time: 46 msec. / elapsed: 50 msec.

-}
main2 :: [Char]
main2 = let
          [send,more,money] = send_more_money
       in
          (show send) ++  "+ " ++ (show more) ++ " = " ++ (show money)
