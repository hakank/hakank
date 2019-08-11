/*

  Olympic puzzle in SWI Prolog

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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall(_, (olympic(Vars),print_olympic(Vars)),_),
        nl.


olympic(Vars) :-

   Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
   Vars ins 1..10,

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
   format("~d  ~d  ~d ~d\n",[X7,X8,X9,X10]),
   format("  ~d  ~d  ~d\n",[X4,X5,X6]),
   format("    ~d  ~d\n",[X2,X3]),
   format("      ~d\n",[X1]),
   nl.


minus(X, Y, Z) :-
   Z #= abs(X-Y).
