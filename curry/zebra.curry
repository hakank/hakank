{- 
  
  Zebra puzzle in Curry

  This is a port my Picat program http://hakank.org/picat/zebra.pi
  """
  Lewis Carrol's classical puzzle with five houses and a zebra:
  
  Five men with different nationalities live in the first five houses
  of a street.  They practise five distinct professions, and each of
  them has a favourite animal and a favourite drink, all of them
  different.  The five houses are painted in different colours.
  
  The Englishman lives in a red house.
  The Spaniard owns a dog.
  The Japanese is a painter.
  The Italian drinks tea.
  The Norwegian lives in the first house on the left.
  The owner of the green house drinks coffee.
  The green house is on the right of the white one.
  The sculptor breeds snails.
  The diplomat lives in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian's house is next to the blue one.
  The violinist drinks fruit juice.
  The fox is in a house next to that of the doctor.
  The horse is in a house next to that of the diplomat.
  
  Who owns a Zebra, and who drinks water?
  """
  

  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Curry page: http://hakank.org/curry/
  
  
-}

import Data.List
-- import Control.AllValues
-- import Control.SetFunctions
import HakankUtils
-- import CLP.FD

right x y l = _ ++ [x,y] ++ _ =:= l 
next x y l = (right x y l) ? (right y x l)


--
-- PAKCS: > 60min
-- KICS2: Main: out of memory Execution time: 481.10s / elapsed: 8:15.11
-- Curry2Go: fatal error: out of memory Execution time: 528.73s / elapsed: 2:33.51
--
zebraProblem =
    let 
      english,spaniard,japanese,italian,norwegian,red,green,white,yellow,blue,painter,sculptor,diplomat,violinist,doctor,dog,snails,fox,horse,zebra,tea,coffee,milk,juice,water free
    in 
      [english, spaniard, japanese, italian, norwegian] =:= makeVars 5 [1..5] &>
      allDifferent3 [english, spaniard, japanese, italian, norwegian] &>

      [red, green, white, yellow, blue] =:= makeVars 5 [1..5] &>
      allDifferent3 [red, green, white, yellow, blue] &>

      [painter, sculptor, diplomat, violinist, doctor] =:= makeVars 5 [1..5] &>
      allDifferent3 [painter, sculptor, diplomat, violinist, doctor] &>

      [dog, snails, fox, horse, zebra] =:= makeVars 5 [1..5] &>
      allDifferent3 [dog, snails, fox, horse, zebra] &>

      [tea, coffee, milk, juice, water] =:= makeVars 5 [1..5] &>
      allDifferent3 [tea, coffee, milk, juice, water] &>

      -- The Englishman lives in a red house.
      english =:= red &>
      
      -- The Spaniard owns a dog.
      spaniard =:= dog &>
      
      -- The Japanese is a painter.
      japanese =:= painter &>
      
      -- The Italian drinks tea.
      italian =:= tea &>
      
      -- The Norwegian lives in the first house on the left.
      norwegian =:= 1 &>
      
      -- The owner of the green house drinks coffee.
      green =:= coffee &>
      
      -- The green house is on the right of the white one.
      green =:= white + 1 &>

      -- The sculptor breeds snails.
      sculptor =:= snails &>
      
      -- The diplomat lives in the yellow house.
      diplomat =:= yellow &>
      
      -- Milk is drunk in the middle house.
      milk =:= 3 &>
      
      -- The Norwegian's house is next to the blue one.
      abs(norwegian - blue) =:= 1 &>
      
      -- The violinist drinks fruit juice.
      violinist =:= juice &>
      
      -- The fox is in a house next to that of the doctor.
      abs(fox - doctor) =:= 1 &>
      
      -- The horse is in a house next to that of the diplomat.
      abs(horse - diplomat) =:= 1 &>
      
      -- Who owns a Zebra, and who drinks water?

      -- Without the list grouping both KICS2 and Curry2Go throw strange errors, which seems to
      -- be caused by the maximum arity was reached.
      -- ([english,spaniard,japanese,italian,norwegian],[red,green,white,yellow,blue],[painter,sculptor,diplomat,violinist,doctor],[dog,snails,fox,horse,zebra],[tea,coffee,milk,juice,water])
      -- Does reducing the output vars speed things up? It seems not...
      (zebra,water)



--
-- OK, let's test list comprehension instead
--
-- PAKCS: 
-- KICS2: 
-- Curry2Go: 
--
zebraProblem2 = [ (zebra,water) |
                  english <- dom, spaniard <- dom, japanese <- dom, italian <- dom, norwegian <- dom,
                  red <- dom, green <- dom, white <- dom, yellow <- dom, blue <- dom,
                  painter <- dom, sculptor <- dom, diplomat <- dom, violinist <- dom, doctor <- dom,
                  dog <- dom, snails <- dom, fox <- dom, horse <- dom, zebra <- dom,
                  tea <- dom, coffee <- dom, milk <- dom, juice <- dom, water <- dom,
                  allDifferent3 [english, spaniard, japanese, italian, norwegian],
                  allDifferent3 [red, green, white, yellow, blue],
                  allDifferent3 [painter, sculptor, diplomat, violinist, doctor],
                  allDifferent3 [dog, snails, fox, horse, zebra],
                  allDifferent3 [tea, coffee, milk, juice, water],
                  
                  -- The Englishman lives in a red house.
                  english == red,
      
                 -- The Spaniard owns a dog.
                 spaniard == dog,
      
                 -- The Japanese is a painter.
                 japanese == painter,
      
                 -- The Italian drinks tea.
                 italian == tea,
      
                 -- The Norwegian lives in the first house on the left.
                 norwegian == 1,

                 -- The owner of the green house drinks coffee.
                 green == coffee,

                 -- The green house is on the right of the white one.
                 green == white + 1,

                 -- The sculptor breeds snails.
                 sculptor == snails,

                 -- The diplomat lives in the yellow house.
                 diplomat == yellow,

                 -- Milk is drunk in the middle house.
                 milk == 3,

                 -- The Norwegian's house is next to the blue one.
                 abs(norwegian - blue) == 1,

                 -- The violinist drinks fruit juice.
                 violinist == juice,

                 -- The fox is in a house next to that of the doctor.
                 abs(fox - doctor) == 1,

                 -- The horse is in a house next to that of the diplomat.
                 abs(horse - diplomat) == 1
                ]
                where dom = [1..5]