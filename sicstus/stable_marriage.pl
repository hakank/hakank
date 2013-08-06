/*

  Stable marriage problem in SICStus Prolog.

  Problem and OPL model from Pascal Van Hentenryck
  "The OPL Optimization Programming Language", page 43ff.

  Also, see 
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/stable_marriage.mzn
  * Comet   : http://www.hakank.org/comet/stable_marriage.co
  * ECLiPSe : http://www.hakank.org/eclipse/stable_marriage.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :- 
        all_solutions(1),
        all_solutions(2),
        all_solutions(3),
        all_solutions(4).


all_solutions(Problem) :-
        format('\nProblem ~d:\n', [Problem]),
        findall([Husband,Wife], 
                stable_marriage(Problem,Husband,Wife)
        ,L),
         ( foreach([H,W], L) do
               write(husband:H),nl,
               write(wife:W),nl,
               nl
         ),fd_statistics.

stable_marriage(Problem,Husband,Wife) :-

        problem(Problem, RankWomen,RankMen),

        matrix(RankWomen,[NumWomen,NumMen]),
        matrix(RankMen,[NumMen,NumWomen]),

        length(Wife,NumMen),
        domain(Wife,1,NumWomen),

        length(Husband,NumWomen),
        domain(Husband,1,NumMen),
        
        all_distinct(Wife, [consistency(domain)]),
        all_distinct(Husband, [consistency(domain)]),

        ( for(M,1,NumMen), param(Husband, Wife) do
              %  Husband[Wife[M]] #= M
              element(M,Wife,WifeM),
              element(WifeM,Husband,HusbandWifeM),
              HusbandWifeM #= M,
              indomain(M)
        ),

        ( for(W,1,NumWomen), param(Husband, Wife) do
              %  Wife[Husband[W]] #= W
              element(W,Husband,HusbandW),
              element(HusbandW,Wife,WifeHusbandW),
              WifeHusbandW #= W,
              indomain(W)
        ),

        ( for(M,1,NumMen),
          param(RankMen,RankWomen,Wife,Husband,NumWomen) do
              ( for(O,1,NumWomen),
                param(RankMen,RankWomen,Wife,Husband,M) do
                    % (RankMen[M,O] #< RankMen[M, Wife[M]]) =>
                    %  (RankWomen[O,Husband[O]] #< RankWomen[O,M])
                    matrix_element(RankMen,M,O,RankMenMO),
                    element(M,Wife,WifeM),
                    matrix_element(RankMen,M,WifeM,RankMenMWifeM),
                    element(O,Husband,HusbandO),
                    matrix_element(RankWomen,O,HusbandO,RankWomenOHusbandO),
                    matrix_element(RankWomen,O,M,RankWomenOM),
                    Reif11 in 0..1,
                    Reif12 in 0..1,
                    RankMenMO #< RankMenMWifeM #<=> Reif11 #= 1,
                    RankWomenOHusbandO #< RankWomenOM #<=> Reif12 #= 1,
                    Reif11 #=> Reif12,
                    (RankMenMO #< RankMenMWifeM) #=>
                    (RankWomenOHusbandO #< RankWomenOM)
              )

        ),

        ( for(W,1,NumWomen),
          param(RankMen,RankWomen,Wife,Husband,NumMen) do
              ( for(O,1,NumMen),
                param(RankMen,RankWomen,Wife,Husband,W) do
                    % (RankWomen[W,O] #< RankWomen[W,Husband[W]]) =>
                    % (RankMen[O,Wife[O]] #< RankMen[O,W])
                    matrix_element(RankWomen,W,O,RankWomenWO),
                    element(W,Husband,HusbandW),
                    matrix_element(RankWomen,W,HusbandW,RankWomenWHusbandW),
                    element(O,Wife,WifeO),
                    matrix_element(RankMen,O,WifeO,RankMenOWifeO),
                    matrix_element(RankMen,O,W,RankMenOW),
                    Reif21 in 0..1,
                    Reif22 in 0..1,
                    RankWomenWO #< RankWomenWHusbandW #<=> Reif21 #= 1,
                    RankMenOWifeO #< RankMenOW #<=> Reif22 #= 1,
                    Reif21 #=> Reif22,
                    (RankWomenWO #< RankWomenWHusbandW) #=>
                    (RankMenOWifeO #< RankMenOW)
              )
         ),

        append(Wife,Husband,Vars),
        labeling([ffc,bisect,up],Vars).

matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



%
% Original problem from van Hentenryck
%
problem(1,
        [[1, 2, 4, 3, 5],  % rankWomen
         [3, 5, 1, 2, 4],
         [5, 4, 2, 1, 3],
         [1, 3, 5, 4, 2],
         [4, 2, 3, 5, 1]],
        
        [[5, 1, 2, 4, 3],  % rankMen
         [4, 1, 3, 2, 5],
         [5, 3, 2, 4, 1],
         [1, 5, 4, 3, 2],
         [4, 3, 2, 1, 5]]).




% Data from
% http://mathworld.wolfram.com/StableMarriageProblem.html
% """
% In the rankings illustrated above, the male-optimal stable 
% marriage is 
%     4, 2, 6, 5, 3, 1, 7, 9, 8, 
% and the female-optimal stable marriage is 
%     1, 2, 8, 9, 3, 4, 7, 6, 5. 
% A stable marriage can be found using StableMarriage[m, w] in the 
% Mathematica package Combinatorica` (which can be loaded with the 
% command <<Combinatorica`)
%
% 
% Note that the matrices given at the MathWorld page are transposed.
% 
% There are 6 solutions, but none is the solution given above:
%
% wife   : [6, 1, 4, 8, 5, 7, 3, 2, 9]
% husband: [2, 8, 7, 3, 5, 1, 6, 4, 9]
%
% wife   : [6, 1, 4, 8, 5, 9, 3, 2, 7]
% husband: [2, 8, 7, 3, 5, 1, 9, 4, 6]
%
% wife   : [6, 4, 1, 8, 5, 7, 3, 2, 9]
% husband: [3, 8, 7, 2, 5, 1, 6, 4, 9]
%
% wife   : [6, 4, 9, 8, 3, 7, 1, 5, 2]
% husband: [7, 9, 5, 2, 8, 1, 6, 4, 3]
%
% wife   : [6, 5, 9, 8, 3, 7, 1, 4, 2]
% husband: [7, 9, 5, 8, 2, 1, 6, 4, 3]
%
% wife   : [7, 5, 9, 8, 3, 6, 1, 4, 2]
% husband: [7, 9, 5, 8, 2, 6, 1, 4, 3]
%
%
% This is the transposed version from the MathWorld page
% 
problem(2, 
        % rankMen = 
        [[7, 3, 8, 9, 6, 4, 2, 1, 5],
         [5, 4, 8, 3, 1, 2, 6, 7, 9],
         [4, 8, 3, 9, 7, 5, 6, 1, 2],
         [9, 7, 4, 2, 5, 8, 3, 1, 6],
         [2, 6, 4, 9, 8, 7, 5, 1, 3],
         [2, 7, 8, 6, 5, 3, 4, 1, 9],
         [1, 6, 2, 3, 8, 5, 4, 9, 7],
         [5, 6, 9, 1, 2, 8, 4, 3, 7],
         [6, 1, 4, 7, 5, 8, 3, 9, 2]],
        % rankWomen =
        [[3, 1, 5, 2, 8, 7, 6, 9, 4],
         [9, 4, 8, 1, 7, 6, 3, 2, 5],
         [3, 1, 8, 9, 5, 4, 2, 6, 7],
         [8, 7, 5, 3, 2, 6, 4, 9, 1],
         [6, 9, 2, 5, 1, 4, 7, 3, 8],
         [2, 4, 5, 1, 6, 8, 3, 9, 7],
         [9, 3, 8, 2, 7, 5, 4, 6, 1],
         [6, 3, 2, 1, 8, 4, 5, 9, 7],
         [8, 2, 6, 4, 9, 1, 3, 7, 5]]).

%
% From 
% http://www.csee.wvu.edu/~ksmani/courses/fa01/random/lecnotes/lecture5.pdf
%
problem(3, 
        % rankMen = 
        [[1,2,3,4],
         [2,1,3,4],
         [1,4,3,2],
         [4,3,1,2]],
        
        % rankWomen =
        [[1,2,3,4],
         [4,3,2,1],
         [1,2,3,4],
         [3,4,1,2]]).


%
% From http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf
% page 4
%
problem(4,
        [[1,5,4,6,2,3],
         [4,1,5,2,6,3],
         [6,4,2,1,5,3],
         [1,5,2,4,3,6],
         [4,2,1,5,6,3],
         [2,6,3,5,1,4]],
        
        [[1,4,2,5,6,3],
         [3,4,6,1,5,2],
         [1,6,4,2,3,5],
         [6,5,3,4,2,1],
         [3,1,2,4,5,6],
         [2,3,1,6,5,4]]).

