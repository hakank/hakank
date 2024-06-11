-- From Sloth sloth-770/tests/Babel/Alice.curry
-- Adjusted to PAKCS

-- import IO

data Mental_state = Sane | Mad deriving(Eq)

neg True = False
neg False = True

is_a Sane = True
is_a Mad = True

is_mad [] = False
is_mad (x:r) = (x==Mad) || (is_mad r)

believes x y =
  if (x==Sane) then y else (neg y)

what_state hatter hare dormouse =
  (is_a hatter & is_a hare && is_a dormouse) &&
  (believes hatter (believes hare (is_mad [hatter,hare,dormouse]))) &&
  (believes dormouse (hare==Sane))

-- alice> main
-- [Mad,Mad,Mad]
main = let hatter, hare, dormouse free in 
           -- printLn $
           what_state hatter hare dormouse =:= True &> [hatter,hare,dormouse]

   
