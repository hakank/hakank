/*

  Crossbar problem in SWI Prolog

  Prolog/FD benchmark problem (crossbar.pl in B Prolog)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        List= [V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,
               V11,V12,V13,V14,V15,V16,V17,V18,V19,V20],
        length(List,N),
        List ins 0..N,

        %% Note:
        %% SWI-Prolog don't accept
        %%     V in [1,2,3,4,5]
        %% Instead one must write
        %%     V in 1\/2\/3\/4\/5
        make_disj_domain(V1, [2,4,6,7,8,9,10,11,16,18,20]),
	make_disj_domain(V2,[2,3,4,8,10,12,17,19,20]),
	make_disj_domain(V3,[2,3,4,6,8,9,11,17,18]),
	make_disj_domain(V4,[1,3,4,5,6,7,9,10,11,13,18]),
	make_disj_domain(V5,[1,5,6,10,12,13,14,17,18,19,20]),
	make_disj_domain(V6,[1,3,10,12,15,16,19,20]),
	make_disj_domain(V7,[5,8,9,10,17]),
        make_disj_domain(V8,[1,2,5,6,7,12,14,15,16,17]),
	make_disj_domain(V9,[1,2,3,4,5,7,11,12,13,14,16,17,20]),
	make_disj_domain(V10,[4,5,8,9,10,11,13,17,18,19,20]),
	make_disj_domain(V11,[2,4,6,7,8,10,12,14,17,18,20]),
	make_disj_domain(V12,[3,7,8,9,10,13,14,15,18,20]),
	make_disj_domain(V13,[2,3,6,7,8,9,11,13,16,20]),
	make_disj_domain(V14,[2,3,5,6,8,9,12,13,15,16,17,18]),
	make_disj_domain(V15,[2,7,8,10,12,13,14,15,16,17,18,20]),
	make_disj_domain(V16,[1,2,6,11,13,16,17,19,20]),
	make_disj_domain(V17,[1,3,6,9,13,19]),
	make_disj_domain(V18,[1,3,6,7,8,10,13,14,19]),
        make_disj_domain(V19,[1,2,3,4,5,6,7,9,11,12,14,16,17,19,20]),
        make_disj_domain(V20,[3,5,6,7,8,9,11,12,13,14,16,18,20]),

	all_different(List),

        label(List),

        writeln(List),
        nl.

