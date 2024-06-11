{- 
  
  Just forgotten puzzle in Curry CLP.FD

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
 
  Joe was furious when he forgot one of his bank account numbers. 
  He remembered that it had all the digits 0 to 9 in some order, so he tried
  the following four sets without success:
 
      9 4 6 2 1 5 7 8 3 0
      8 6 0 4 3 9 1 2 5 7 
      1 6 4 0 2 9 7 8 5 3
      6 8 2 4 3 1 9 0 7 5
 
  When Joe finally remembered his account number, he realised that in each set
  just four of the digits were in their correct position and that, if one knew
  that, it was possible to work out his account number.
  What was it? 
  """

  This is a port of my Picat model http://hakank.org/picat/just_forgotten.pi

  NOPE. This does not work!


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
-- import HakankUtils
import CLP.FD

tests = [[1,5,9,3,6,7,2,0,4,8],
         [9,7,3,2,5,1,6,0,4,8],
         [5,4,1,8,3,2,7,9,6,0],
         [7,5,0,8,3,2,9,6,4,1]]

{-
just_forgotten.curry:43:24-43:78 Error:
    Type error in infix application
    CLP.FD.sum [(fd r) =# x | (x, r) <- zip xs row] =# (fd 4)
     
    Term: CLP.FD.sum [(fd r) =# x | (x, r) <- zip xs row]
    Inferred type: CLP.FD.FDRel -> CLP.FD.FDExpr -> CLP.FD.FDConstr
    Expected type: CLP.FD.FDExpr
    Types CLP.FD.FDExpr
      and CLP.FD.FDRel -> CLP.FD.FDExpr -> CLP.FD.FDConstr
    are incompatible
    | 
 43 | check xs (row:rows) = (CLP.FD.sum [(fd r) =# x| (x,r) <- zip xs row] =# (fd 4)) /\ check xs rows
    |                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
-- check xs (row:rows) = (CLP.FD.sum [(fd r) =# x| (x,r) <- zip xs row] =# (fd 4)) /\ check xs rows


just_forgotten0 = let
                    xs = take 10 (domain 0 9)
                    t0 = [1,5,9,3,6,7,2,0,4,8]
                 in
                    solveFD [] xs $
                    allDifferent xs /\
                    
                    -- check xs tests
                    -- Nope: Types CLP.FD.FDExpr and CLP.FD.FDConstr are incompatible
                    -- foldl1 (/\) [ Data.List.sum [x =# (fd r) | (x,r) <- zip xs row] =# (fd 4) | row <- tests]
                    -- Just the first row...
                    -- Data.List.sum [ x =# (fd r) | (x,r) <- zip xs (tests !! 0)] =# (fd 4)
                    -- CLP.FD.sum [ (x =# (fd r)) | (x,r) <- zip xs (tests !! 0)] Equ (fd 4)

                    -- Testing even smaller: NOPE! Same error
                    -- ( (xs !! 0) =# (t0 !! 0)) + ((xs !! 1) =# (t0 !! 1)) >=# 0




main = just_forgotten
                    
