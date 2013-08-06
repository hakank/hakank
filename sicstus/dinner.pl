/*

  Some dinner problem in SICStus Prolog.

  http://www.sellsbrothers.com/spout/#The_Logic_of_Logic
  """
  My son came to me the other day and said, "Dad, I need help with a"
  "math problem." The problem went like this:

    * We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
    * Grandparents cost $3 for dinner, parents $2 and children $0.50
    * There must be 20 total people at dinner and it must cost $20
    * How many grandparents, parents and children are going to dinner?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/dinner.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/dinner.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


solve([Grandparents, Parents, Children]) :-
        Grandparents in 0..100,
        Parents      in 0..100,
        Children     in 0..100,
        Grandparents * 3 + Parents * 2 + Children / 2 #= 20,

        Grandparents + Parents + Children #= 20, % number of people = 20

        % must be some of each
        Grandparents #> 0,
        Parents      #> 0,
        Children     #> 0,
        
        labeling([],[Grandparents, Parents, Children]).
        

go :-
        findall([grandparents:Grandparents, 
                 parents:Parents, 
                 children:Children], 
                solve([Grandparents,
                       Parents,
                       Children]),
                L),
        write(L),nl.
