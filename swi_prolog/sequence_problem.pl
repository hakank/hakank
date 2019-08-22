/*

  Sequence problem in SWI Prolog

  From Marriott, Stuckey: "Programming with Constraints", pages 31ff, 294f

  Combine the three contigs
    [a,t,c,g,g,g,c],[a,a,a,a,t,c,g],[g,c,c,a,t,t]
  to an overlapping sequence:

     AAAATCG
       ATCGGGC
            GCCATT

  i.e. the sequence AAAATCGGGCCATT


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

%% :- use_module(library(clpfd)).
%% :- use_module(hakank_utils).

go :-
        gp295(T),
        writeln(T),
        fail.

go.

%%
%% Sequence constraints: not_empty and concatentation using lists p294.
%%
not_empty([_|_]).

concat([S1],S1).
concat([S1,S2|Ss],S) :- append(S1,T,S), concat([S2|Ss],T).

%% sequence problem goal p295.
gp295(T) :- sequence_problem(T).

%%
%% The sequence problem as a constraint program p295.
%%
sequence_problem(T) :- 
    	contigs(Contigs), 
    	perm(Contigs, [C1, C2, C3]),
    	not_empty(O12), not_empty(O23),
    	concat([UC1,O12], C1),
    	concat([O12,UC2,O23], C2),
    	concat([O23,UC3], C3),
    	concat([UC1,O12,UC2,O23,UC3], T).
 
contigs([[a,t,c,g,g,g,c], [a,a,a,a,t,c,g], [g,c,c,a,t,t]]). 
 
delete2([X | Xs], X, Xs). 
delete2([X | Xs], Y, [X | R]) :- delete2(Xs, Y, R). 
 
perm([], []). 
perm(L, [X|R]) :- delete2(L, X, L1), perm(L1, R). 
 


        