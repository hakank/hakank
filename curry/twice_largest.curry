{-

  Twice largest in SWI Prolog

  https://theweeklychallenge.org/blog/perl-weekly-challenge-191/#TASK1
  """
  Task 1: Twice Largest
Submitted by: Mohammad S Anwar

You are given list of integers, @list.

Write a script to find out whether the largest item in the list is at least twice as large as each of the other items.
Example 1

Input: @list = (1,2,3,4)
Output: -1

The largest in the given list is 4. However 4 is not greater than twice of every remaining elements.
1 x 2 <= 4
2 x 2 <= 4
2 x 3 >  4

Example 2

Input: @list = (1,2,0,5)
Output: 1

The largest in the given list is 5. Also 5 is greater than twice of every remaining elements.
1 x 2 <= 5
2 x 2 <= 5
0 x 2 <= 5

Example 3

Input: @list = (2,6,3,1)
Output: 1

The largest in the given list is 6. Also 6 is greater than twice of every remaining elements.
2 x 2 <= 6
3 x 2 <= 6
1 x 2 <= 6

Example 4

Input: @list = (4,5,2,3)
Output: -1

The largest in the given list is 5. Also 5 is not greater than twice of every remaining elements.
4 x 2 >  5
2 x 2 <= 5
3 x 2 >  5
  """

  Via
  http://www.rabbitfarm.com/cgi-bin/blosxom/2022/11/20

  Cf ~/swi_prolog/me/twice_largest.pl  

  Program created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Curry page: http://www.hakank.org/curry/

-}

import Data.List

tests =  [[1,2,3,4],
          [1,2,0,5],
          [4,5,2,3]]

twice_largest x = m >= 2*rest
                  where m = maximum x
                        rest = maximum $ delete m x


main = do
         print main1
         print main2
         print main3

main1 = filter twice_largest tests

main2 = map twice_largest tests

main3 = map (\x -> (x,if twice_largest x then True else False)) tests
