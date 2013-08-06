/*

  Olympic puzzle in SICStus Prolog.

  Benchmark for Prolog (BProlog)
  """
    File   : olympic.pl
    Author : Neng-Fa ZHOU
    Date   : 1993
 
    Purpose: solve a puzzle taken from Olympic Arithmetic Contest
 
     Given ten variables with the following configuration:
 
                 X7   X8   X9   X10
 
                    X4   X5   X6
 
                       X2   X3             
 
                          X1
 
    We already know that X1 is equal to 3 and want to assign each variable
    with a different integer from {1,2,...,10} such that for any three
    variables 
                        Xi   Xj
 
                           Xk
    the following constraint is satisfied:
 
                      |Xi-Xj| = Xk
  """
  

  Compare with the following model:
  * MiniZInc: http://www.hakank.org/minizinc/olympic.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        findall(_, (olympic(Vars),print_olympic(Vars)),_),
        fd_statistics.


olympic(Vars) :-
        R = 10,
        Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
        domain(Vars,1,10),

        all_different(Vars),

        X1 #= 3,
        minus(X2,X3,X1),
        minus(X4,X5,X2),
        minus(X5,X6,X3),
        minus(X7,X8,X4),
        minus(X8,X9,X5),
        minus(X9,X10,X6),

        labeling([ff], Vars).


print_olympic([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]) :-
        format('~d  ~d  ~d ~d\n',[X7,X8,X9,X10]),
        format('  ~d  ~d  ~d\n',[X4,X5,X6]),
        format('    ~d  ~d\n',[X2,X3]),
        format('      ~d\n',[X1]),
        nl.


minus(X, Y, Z) :-
   Z #= abs(X-Y).


