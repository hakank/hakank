{-
  Palindromes in Curry
-}

import Data.List
import HakankUtils

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

genPalin n = x =:= makeVars n ['a'..'z'] & palindrome x where x free

main = palindrome "anna"

main2 = (x =:= makeVars 4 ['a'..'h'] & palindrome x) `seq` x where x free

main3 = (genPalin 3) `seq` x where x free

