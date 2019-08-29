/*

  Global constraint all_differ_from_at_least_k_pos in SWI Prolog

  From Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Call_different_from_at_least_k_pos.html
  """
  Enforce all pairs of distinct vectors of the VECTORS collection to differ 
  from at least K positions.
  
  Example
  (
   2, <
   vec-<2, 5, 2, 0>,
   vec-<3, 6, 2, 1>,
   vec-<3, 6, 1, 0>
   >
 )
  
  The all_differ_from_at_least_k_pos constraint holds since:
   * The first and second vectors differ from 3 positions, which is 
     greater than or equal to K=2.
   * The first and third vectors differ from 3 positions, which is greater 
     than or equal to K=2.
   * The second and third vectors differ from 2 positions, which is greater 
     than or equal to K=2.
  """

  Note:
  all_differ_from_at_least_k_pos/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   Rows = 3,
   Cols = 4,

   new_matrix(Rows,Cols,0..6,X),
   flatten(X,XList),

   K in 0..Cols,
   K #= 2,

   % the example above
   % X = [[2,5,2,0],
   %      [3,6,2,1],
   %      [3,6,1,0]],

   % X = [[2,5,2,_],
   %      [3,6,2,1],
   %      [3,6,1,0]],

   X = [[3,6,3,_],
        [3,6,2,1],
        [3,6,1,0]],


   % this fails
   % X = [[2,5,2,0],
   %      [2,5,2,1],
   %      [3,6,1,0]],

   all_differ_from_at_least_k_pos(K,X),

   flatten([XList,K],Vars),
   labeling([],Vars),
   maplist(writeln,X),
   nl.

%%
%% Require that all rows should be completely different
%%
go2 :-
        N = 3,

        new_matrix(N,N,1..N,X),

        % K in 1..N,
        K #= 3,

        findall(X,(all_differ_from_at_least_k_pos(K,X),flatten(X,Vars), label(Vars)),L),
        length(L,Len),
        maplist(writeln,L),
        writeln(len=Len),
        nl.

        
%%
%% Require that all rows and columns should differ by K positions.
%%
go3 :-
        N = 3,

        new_matrix(N,N,1..N,X),
        transpose(X,XT),
        

        % K in 1..N,
        K #= 2,

        findall(X,
                (all_differ_from_at_least_k_pos(K,X),
                 all_differ_from_at_least_k_pos(K,XT),
                 flatten(X,Vars),
                 label(Vars)),
                L),
        length(L,Len),
        maplist(writeln,L),
        writeln(len=Len),
        nl.

        
