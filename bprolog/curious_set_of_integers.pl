/*

  Curious set of integers problem in B-Prolog.

  Martin Gardner (February 1967):
  """
  The integers 1,4,9, and 120 form a set with a remarkable property:
  the product of any two integers is one less than a perfect square. 
  Find a fifth number that can be added to the set without destroying 
  this property.
  """

  Solution: The number is 0.
 
  There are however other sets of five numbers with this property.
  Here are the one in the range of 0.10000:

  [0, 1, 3, 8, 120]
  [0, 1, 3, 120, 1680]
  [0, 1, 8, 15, 528]
  [0, 1, 8, 120, 4095]
  [0, 1, 15, 24, 1520]
  [0, 1, 24, 35, 3480]
  [0, 1, 35, 48, 6888]
  [0, 2, 4, 12, 420]
  [0, 2, 12, 24, 2380]
  [0, 2, 24, 40, 7812]
  [0, 3, 5, 16, 1008]
  [0, 3, 8, 21, 2080]
  [0, 3, 16, 33, 6440]
  [0, 4, 6, 20, 1980]
  [0, 4, 12, 30, 5852]
  [0, 5, 7, 24, 3432]
  [0, 6, 8, 28, 5460]
  [0, 7, 9, 32, 8160]


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


go :-
        % time2(curious(X,constr,split)),
        % time2(curious(X,degree,split)),
        time2(curious(X,ffc,split)),
        writeln(X).


curious(X,VarSel,ValSel) :-

        N = 5, 
        length(X, N),
        DomMax = 1000,
        X :: 0..DomMax,

        alldistinct(X),
        increasing(X, N),

        foreach(I in 1..N, J in 1..I-1,
                [P],ac(Ps,[]),
                (
                    P :: 0..DomMax,
                    Ps^1 = [P|Ps^0],
                    P*P-1 #= X[I]*X[J]
                )
               ),

        term_variables([X,Ps], Vars),
        labeling([VarSel,ValSel],Vars).




increasing(List, N) :-
        foreach(I in 2..N, List[I-1] #=< List[I]).
