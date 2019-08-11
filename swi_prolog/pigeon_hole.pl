/*

  Pigeon hole problem in SWI Prolog

  From
  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes 
  (at most 1 pigeon per hole). The boolean formulation uses n - m variables to 
  indicate, for each pigeon, its hole number. Obviously, there is a 
  solution iff n <= m.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :- 
        N = 3, %% N pigeons
        M = 10,%% 10 %% M pigeon holes     
        wrapper(N,M),
        nl.


%% This is an impossible problem (M < N)
go2 :-
        N = 5, %% N pigeons
        M = 4, %% M pigeon holes
        wrapper(N,M).


wrapper(N,M) :-
        writeln([n=N,m=M]),
        findall(P, pigeon_hole(N,M, P),L),
        length(L,Len),
        maplist(writeln,L),
        format("It was ~d solutions.~n", Len).

pigeon_hole(N,M, PigeonHoles) :-
        %% N pigeons at M pigeon holes
        new_matrix(N,M,0..1, PigeonHoles),
        
        %% all pigeon must be placed and only at one hole (rows)
        maplist(sums(#=,1),PigeonHoles),
        
        %% max 1 pigeon per pigeon hole (columns)
        transpose(PigeonHoles,PigeonHolesT),
        maplist(sums(#=<,1),PigeonHolesT),        

        
        flatten(PigeonHoles,Vars),
        labeling([],Vars).


pretty_print(X) :-
        writeln(pretty_print(X)),
        maplist(writeln,X),
        nl.

sums(Rel,Value,L) :-
        sum(L,Rel,Value).
