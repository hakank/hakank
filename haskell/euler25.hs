{- 
  
  Euler #25 in Haskell

  """
  The Fibonacci sequence is defined by the recurrence relation:

     Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
  
  Hence the first 12 terms will be:

     F1 = 1
     F2 = 1
     F3 = 2
     F4 = 3
     F5 = 5
     F6 = 8
     F7 = 13
     F8 = 21
     F9 = 34
     F10 = 55
     F11 = 89
     F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?")
  """


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import HakankUtils

fibDigits = length . show . fib


euler25a = check_a 1
           where check_a n | fibDigits n >= 1000 = n
                           | otherwise = check_a (n+1)

euler25b = 1 + (last $ takeWhile (\n -> fibDigits n < 1000) [1..])

euler25c = head $ dropWhile (\n -> fibDigits n < 1000) [1..]

--
-- A more "intelligent" search. Kind of bin search...
--
iSearch f n target step aux | fn == target = (True,n) : aux
                         | fn < target = (False, n) : iSearch f (n+1+step) target step aux
                         | otherwise = (False, n) : iSearch f (n-step) target step aux
                         where fn = f n


--
-- This happens to work for this specific stepsize (43).
-- For other stepsizes the result might be a number that has 1000 digits but is larger than 4782...
-- iSearch (\n -> fibDigits n) 1 1000 43 []
-- [(False,1),(False,45),(False,89),(False,133),(False,177),(False,221),(False,265),(False,309),(False,353),(False,397),(False,441),(False,485),(False,529),(False,573),(False,617),(False,661),(False,705),(False,749),(False,793),(False,837),(False,881),(False,925),(False,969),(False,1013),(False,1057),(False,1101),(False,1145),(False,1189),(False,1233),(False,1277),(False,1321),(False,1365),(False,1409),(False,1453),(False,1497),(False,1541),(False,1585),(False,1629),(False,1673),(False,1717),(False,1761),(False,1805),(False,1849),(False,1893),(False,1937),(False,1981),(False,2025),(False,2069),(False,2113),(False,2157),(False,2201),(False,2245),(False,2289),(False,2333),(False,2377),(False,2421),(False,2465),(False,2509),(False,2553),(False,2597),(False,2641),(False,2685),(False,2729),(False,2773),(False,2817),(False,2861),(False,2905),(False,2949),(False,2993),(False,3037),(False,3081),(False,3125),(False,3169),(False,3213),(False,3257),(False,3301),(False,3345),(False,3389),(False,3433),(False,3477),(False,3521),(False,3565),(False,3609),(False,3653),(False,3697),(False,3741),(False,3785),(False,3829),(False,3873),(False,3917),(False,3961),(False,4005),(False,4049),(False,4093),(False,4137),(False,4181),(False,4225),(False,4269),(False,4313),(False,4357),(False,4401),(False,4445),(False,4489),(False,4533),(False,4577),(False,4621),(False,4665),(False,4709),(False,4753),(False,4797),(False,4754),(False,4798),(False,4755),(False,4799),(False,4756),(False,4800),(False,4757),(False,4801),(False,4758),(False,4802),(False,4759),(False,4803),(False,4760),(False,4804),(False,4761),(False,4805),(False,4762),(False,4806),(False,4763),(False,4807),(False,4764),(False,4808),(False,4765),(False,4809),(False,4766),(False,4810),(False,4767),(False,4811),(False,4768),(False,4812),(False,4769),(False,4813),(False,4770),(False,4814),(False,4771),(False,4815),(False,4772),(False,4816),(False,4773),(False,4817),(False,4774),(False,4818),(False,4775),(False,4819),(False,4776),(False,4820),(False,4777),(False,4821),(False,4778),(False,4822),(False,4779),(False,4823),(False,4780),(False,4824),(False,4781),(False,4825),(True,4782)]
-- 
euler25d = snd $ last $ iSearch (\n -> fibDigits n) 1 1000 43 []


main = do
         -- print euler25a -- (2.14 secs, 4,430,960,144 bytes)

         -- print euler25b -- (2.07 secs, 4,365,838,064 bytes)

         -- print euler25c -- (2.11 secs, 4,365,373,656 bytes)
         
         print euler25d -- (0.13 secs, 226,369,752 bytes)