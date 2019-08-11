/*

  Decomposition of global contiguity using regular constraint in SWI Prolog

  Here's a variant of the global constraint global contiguity
  using (a decomposition of) the regular constraint.

  See http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  for a description of the constraint.

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 15,
   length(X,N),
   X ins 0..1,

   % Constraints
   global_contiguity(X),

   labeling([ff], X),

   writeln('x  '=X),
   fail,
   nl.

go.


global_contiguity(X) :-

   length(X,N),

   % Transition function (MiniZinc style)  
   % This use the regular expression "0*1*0*" to 
   % require that all 1's (if any) in an array appear contiguously.
   % 
   Transition = [
                 [1,2], % state 1 (start) input 0 -> state 1, input 1 -> state 2 i.e. 0*
                 [3,2], % state 2: 1*
                 [3,0]  % state 3: 0*
                ],
   NStates = 3,
   InputMax = 2,
   InitialState = 1,
   AcceptingStates = [1,2,3],

   length(RegInput,N),
   RegInput ins 1..InputMax,  % 1..2

   % Translate X's 0..1 to RegInput's 1..2
   % foreach(I in 1..N) 
   %    RegInput[I] #= X[I]+1  
   % end,
   maplist(add1,X,RegInput),
   
   regular(RegInput,NStates,InputMax,Transition,InitialState, AcceptingStates).

add1(X,Y) :- Y #= X + 1.
