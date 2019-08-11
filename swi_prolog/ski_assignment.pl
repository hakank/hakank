/*

  Ski assignment in SWI Prolog

  From Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008, Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   SkiHeights   = [1, 2, 5, 7, 13, 21],
   SkierHeights = [3, 4, 7, 11, 18],
   
   length(SkiHeights, NumSkis),
   length(SkierHeights,NumSkiers),

   length(X,NumSkiers), 
   X ins 1..NumSkis,
   
   all_different(X),

   % minimize the differences of ski height and skier's height (Z)
   numlist(1,NumSkiers,Is),
   check_heights(Is,X,SkierHeights,SkiHeights,0,Z),
   
   labeling([min(Z)],X),

   writeln(x=X),
   writeln(z=Z).


check_heights([],_X,_SkierHeights,_SkiHeights,Sum,Sum).
check_heights([I|Is],X,SkierHeights,SkiHeights,Sum0,Sum) :-
        element(I,X,XI),
        element(XI,SkiHeights,SXI),
        element(I,SkierHeights,SXI2),
        Sum1 #= Sum0 + abs(SXI - SXI2),
        check_heights(Is,X,SkierHeights,SkiHeights,Sum1,Sum).
        
