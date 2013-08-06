/*

  Ski assignment problem in SICStus Prolog.

  From  
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008, Final Review, December 12, 2008
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

  Compare with the following models:
  MiniZinc: http://www.hakank.org/minizinc/ski_assignment_problem.mzn
  Comet   : http://www.hakank.org/comet/ski_assignment.co
  ECLiPSe : http://www.hakank.org/eclipse/ski_assignment.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        SkiHeights =   [1, 2, 5, 7, 13, 21],
        SkierHeights = [3, 4, 7, 11, 18],

        length(SkiHeights,NumSkis),
        domain(SkiHeights,0,100),

        length(SkierHeights,NumSkiers),
        domain(SkierHeights,0,100),

        % the assignments
        length(X,NumSkiers), 
        domain(X,1,NumSkis),

        % minimize the differences of ski height and skier's height (Z)
        Z in 0..10,
        ( for(I,1,NumSkiers),
          fromto(0,In,Out,Z),
          param(SkiHeights,SkierHeights,X) do
              element(I,X,XI),
              element(XI,SkiHeights,SkiHeightsXI),
              element(I,SkierHeights,SkierHeightsI),
              Out #= In + abs(SkiHeightsXI - SkierHeightsI)
        ),
          
        labeling([ff,bisect,down,minimize(Z)], X),
        
        write(x:X),nl,
        write(z:Z),nl,
        fd_statistics.
