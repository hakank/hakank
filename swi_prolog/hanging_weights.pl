/*

  Hanging weights problem in SWI Prolog

  From 
  "Using LINQ to solve puzzles"
  http://blogs.msdn.com/lukeh/archive/2007/03/19/using-linq-to-solve-puzzles.aspx
  """
  Here's a puzzle similar to the one in the puzzle hunt.  The diagram 
  below is a bunch of weights (A-M) hanging from a system of bars.  
  Each weight has an integer value between 1 and 13, and the goal is 
  to figure out what each weight must be for the the diagram below to 
  balance correctly as shown: 

                           |
                           |
               +--+--+--+--+--+--+--+
               |                    |
               |                    |
            +--+--+--+--+--+        |
            |     L        M        |
            |                       |
   +--+--+--+--+--+--+     +--+--+--+--+--+
   H              |  I     |  J        K  |
                  |        |              |
         +--+--+--+--+--+  |     +--+--+--+--+--+
         E              F  |     G              |
                           |                    |
               +--+--+--+--+--+  +--+--+--+--+--+--+
               A              B  C                 D

  The rules for this kind of puzzle are: 
  (1) The weights on either side of a given pivot point must be equal, 
      when weighted by the distance from the pivot, and 
  (2) a bar hanging beneath another contributes it's total weight as 
      through it were a single weight.  For instance, the bar on the bottom 
      right must have 5*C=D, and the one above it must have 3*G=2*(C+D).
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall([All, total=Total], hanging_weights(All,Total), Result),
        maplist(writeln,Result),
        nl.

hanging_weights(All, Total) :-

        All = [A,B,C,D,E,F,G,H,I,J,K,L,M],
        All ins 1..13,

        all_different(All),
        sum(All,#=,Total),
        
        4 * A #= B, 
        5 * C #= D, 
        3 * E #= 2 * F, 
        3 * G #= 2 * (C + D), 
        3 * (A + B) + 2 * J #= K + 2 * (G + C + D), 
        3 * H #= 2 * (E + F) + 3 * I, 
        (H + I + E + F) #= L + 4 * M, 
        4 * (L + M + H + I + E + F) #= 3 * (J + K + G + A + B + C + D),

        flatten([All,Total], Vars),
        labeling([],Vars).
        