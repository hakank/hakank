/*

  Global constraint alldifferent_cst in SWI Prolog

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
  """
  For all pairs of items (VARIABLES[i], VARIABLES[j]) (i!=j) of the 
  collection VARIABLES enforce 
  VARIABLES[i].var+VARIABLES[i].cst != VARIABLES[j].var+VARIABLES[j].cst.
 
  Example
   (<
      var-5 cst-0,
      var-1 cst-1,
      var-9 cst-0,
      var-3 cst-4
   >
   )
  
  The alldifferent_cst constraint holds since all the expressions 
  5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
  """

  Note: alldifferent_cst/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   N = 4,

   Cst = [0,1,0,4],
   % Cst = [0,0,0,0], % for plain all_different 

   length(X,N),
   X ins 1..9,

   % X = [5,1,9,3],

   findall([X,Cst], 
           (alldifferent_cst(X, Cst),
            flatten([X,Cst], Vars),
            label(Vars)),L),
   length(L,Len),
   
   maplist(print_result,L),
   write(len=Len),
   nl.


%%
%% Here both X and Cst are free variables.
%%
go2 :-
   N = 4,

   length(Cst,N),
   Cst ins 1..3,

   length(X,N),
   X ins 1..5,

   findall([X,Cst], 
           (alldifferent_cst(X, Cst),
            flatten([X,Cst], Vars),
            label(Vars)),L),
   length(L,Len),
   
   maplist(print_result,L),
   write(len=Len),
   nl.


print_result([X,C]) :-
        maplist(plus,X,C,Res),
        writeln([x=X,cst=C,res=Res]).


