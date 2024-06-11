{-
  Exercise 1 from Curry: A Tutorial Introduction, page 12
  """
  Exercise 1 Define a predicate, read as “factors” and denoted by the infix operator 
  "./.", 
  that tells whether an integer is a factor of another integer. The predicate should 
  work for every input and 0 should not be a factor of any integer. 
  The operator should be non-associative and have precedence 7. 
  """
-}

-- non-assoc and precedence 7
infix 7  ./.

-- (./.) :: Num a => a -> a -> Bool -- "Data signature too general"
(./.) :: Int -> Int -> Bool -- "Data signature too general"
x ./. 0 = False
-- We must guard against 0 as a divisor.
x ./. y | y > 0 = x `mod` y == 0

main :: Bool
-- -> False
main = 10 ./. 3

-- main1 :: Num a => ((Int,[Char],a),Int,Bool) -- Well, I might change this later on...
-- ->
-- ((0,"./.",10),0,False)
-- ((1,"./.",10),1,True)
-- ((2,"./.",10),2,True)
-- ((3,"./.",10),3,False)
-- ((4,"./.",10),4,False)
-- ((5,"./.",10),5,True)
main1 = (x =:= (0?1?2?3?4?5) &> y =:= 10 ./. x) &> y =:= True &> ((x,"./.",10),x,y) where x,y free

-- This works in KiCS2, but PAKCS just complain about suspended constraints
main2 = (y =:= 10 ./. x) &> y =:= True &> ((x,"./.",10),x,y) where x,y free