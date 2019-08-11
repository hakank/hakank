/*

  Set covering problem in SWI Prolog

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such 
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%
% First find the optimal value (MinVal), then find all the solutions with that value.
%
go :-

   writeln("Find the optimal solution"),
   belongs(Belongs),
   set_covering_skiena(Belongs, MinVal,_),

   format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal]),
   findall(X, set_covering_skiena(Belongs,  MinVal,X),L),
   length(L,Len),
   maplist(writeln,L),
   format("It was ~d solutions\n", [Len]),
   nl.


set_covering_skiena(Belongs, MinVal, X) :-

   length(Belongs,NumSets),

   length(X,NumSets),
   X ins 0..1,

   transpose(Belongs,BelongsTransposed),
   maplist(check_sum(X),BelongsTransposed),
   sum(X,#=,MinVal),

   
   % either search for all solutions (for the minimum value) or
   % the optimal value
   ( ground(MinVal)
   ->
       labeling([],X)
   ;
     labeling([min(MinVal)], X)
   ).

check_sum(X,B) :-
        maplist(x_times_b,X,B,Z),
        sum(Z,#>=,1).

%% Z = X if B = 1, else 0
x_times_b(X,B,Z) :-
        B #= 1 #==> Z #= X,
        B #= 0 #==> Z #= 0.



%
% The belong matrix:
%
belongs(Belongs) :- 
        Belongs = 
        [[1,1,0,0,0,0,0,0,0,0,0,0],
         [0,1,0,0,0,0,0,1,0,0,0,0],
         [0,0,0,0,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,1,1,0,0,1,1,0],
         [0,0,0,0,0,0,0,0,1,1,0,0],
         [1,1,1,0,1,0,0,0,1,1,1,0],
         [0,0,1,1,0,0,1,1,0,0,1,1]].
