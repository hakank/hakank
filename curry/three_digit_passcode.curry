{-
  3 digit passcode in Curry.

  From https://www.reddit.com/r/prolog/comments/14zi1gn/can_we_solve_this_using_prolog_lets_organize_a/
  """
  Can we solve this using Prolog? Let's organize a code golf challenge for it.

  Agent Monocle is hacking into a secret database and has one last chance to crack the
  code. Of the numbers she's tried below each guess has just one correct digit that's
  in the correct order. Knowing that, can you figure out the 3-digit passcode?

  Passcode: 896 X
  Passcode: 983 X
  Passcode: 246 X
  Passcode: 843 X
  Passcode: *** V
  """

  Note: I first named this program 3_digit_passcode.curry but got an error:
  '''
   module name expected
   | 
   import 3_digit_passcode
  ---
 
  since a module name must start with an alpha.

  Cf ~/picat/me_3_digit_passcode.pi
     ~/swi_prolog/me_3_digit_passcode.pi


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Picat page: http://www.hakank.org/picat/
   
-}

import Data.List

passcodes =  [[8,9,6],
              [9,8,3],
              [2,4,6],
              [8,4,3]]

-- Check that each passcode match with x in exactly one position
check _  []          = 0
check xs (ps:pss)    = (if check1 xs ps == 1 then 1 else 0) + check xs pss

-- Sum the number of matches xs == ps
check1 [] []         = 0
check1 (x:xs) (p:ps) = (if x == p then 1 else 0) + check1 xs ps

-- Reduce the domains of the possible values
-- > dom passcodes
-- [2,3,4,6,8,9]
dom = nub . sort . concat

vars 0 xs = []
vars n xs | n > 0 = (anyOf xs) : vars (n-1) xs

-- Is it faster using &> instead of & (or &&)? Cannot see any difference here...
main = (x1 =:= anyOf d &
        x2 =:= anyOf d &
        x3 =:= anyOf d &
        length passcodes =:= check [x1,x2,x3] passcodes) `seq` [x1,x2,x3]
        where
           d = dom passcodes
           x1,x2,x3 free

-- Using vars makes it a little cleaner 
main2 = (x =:= vars 3 d &
        length passcodes =:= check x passcodes) `seq` x
        where
           d = dom passcodes
           x free
