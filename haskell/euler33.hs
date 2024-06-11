{- 
  
  Euler #33 in Haskell

  """
  The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
  attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, 
  is obtained by cancelling the 9s.

  We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than 
  one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find 
  the value of the denominator.
  """ 

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
-- import HakankUtils

--
-- It took a while to get all the types and fromIntegral correct...
--

calc_a :: (Integral a, Integral b, Fractional c) => a -> b -> c
calc_a y z = 9.0*yi*zi/(10.0*yi-zi) 
           where
           yi = fromIntegral y
           zi = fromIntegral z

check_a :: Float -> Int -> Int -> Bool
check_a x y z = 1.0*(fromIntegral $ floor(x))==1.0*x && (fromIntegral y)/(fromIntegral z) < 1.0 && x < 10.0

euler33a :: Prelude.Integral a => a
euler33a = floor $ foldl (/) 1 [yz | y<-[1..9], z <- [y..9], let x = calc_a y z, check_a x y z, let yz = (fromIntegral y) / (fromIntegral z)]

main :: IO ()
main = do
          print euler33a -- (0.01 secs, 160,776 bytes)
