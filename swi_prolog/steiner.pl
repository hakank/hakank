/*

  Steiner triplets in SWI Prolog

  http://www.probp.com/examples/clpset/steiner.pl
  """
  The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements 
  in {1,2,...,n} such that each set contains three elements and any two 
  sets have at most one element in common.

  For example, the following shows a solution for size n=7:

      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

  Problem taken from:
  C. Gervet: Interval Propagation to Reason about Sets: Definition and 
             Implementation of a PracticalLanguage,  
             Constraints, An International Journal, vol.1, pp.191-246, 1997.
  """


  Note: This model uses arrays of booleans as an representation of sets.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%%
%% Steiner(7)
%% - with symmetry breaking (ordering of the sets): 30 solutions.
%% - w/o symmetry breaking (ordering): 64800 solutions
%% 
go :-
        %% The possible number of Steiner triplets
        findall(I,
                (between(1,40,I),
                 I mod 6 #= 1 #\/ I mod 6 #= 3
                ),
                AllowedN),
        writeln(allowedN=AllowedN),
        
        %% Here we test steiner(7)
        N = 7,
        writeln(n=N),
        steiner(N,Steiner),
        writeln(Steiner),nl,
        nl,
        % fail,
        nl.

go.


%%
%% Check first solution for N in 3..25
%%
%%
go2 :-
        between(3,25,N),
        Mod #= N mod 6,
        (Mod #= 1; Mod #= 3),
        writeln(n=N),
        (
         time(once(steiner(N,Steiner))),
         nonvar(Steiner)
        ->
         writeln(steiner=Steiner)
        ;
         true
        ),
        nl,
        fail,
        nl.

go2.


%%
%% Number of solutions for N=7: 64800 solutions
%%
%% % 5,402,538,263 inferences, 310.726 CPU in 310.726 seconds (100% CPU, 17386822 Lips)
%%
go3 :-
        N = 7,
        time(findall(_, steiner(N,_Steiner),L)),
        length(L,Len),
        format("For N=7: ~w solutions~n",[Len]),
        nl.


%%
%% steiner(N,Steiner)
%%
steiner(N,Steiner) :-
         Mod #= N mod 6,
         (
          \+ (Mod == 1; Mod == 3)
         ->
          writeln("N must be (1|3) modulo 6"),     
          fail
         ;
          true
         ),
         
         %% number of sets
         Nb #= (N * (N-1)) // 6,
         
         new_matrix(Nb,N,0..1, Sets),
         flatten(Sets,SetsList),

         %% symmetry breaking
         matrix_element(Sets,1,1,1),

         %% any two sets can have atmost 1 element in common
         numlist(1,Nb,Is),
         maplist(atmost_1_in_common(Sets),Sets,Is),
         
         writeln(search),
         labeling([max,down,bisect],SetsList),

         findall(Ks,
                 (member(S,Sets),
                  findall(K,
                          (between(1,N,K),
                           nth1(K,S,1)
                          ),
                          Ks
                         )
                 ),
                 Steiner
                ).



%%
%% atmost 1 element in common
%%
atmost_1_in_common(Sets,SetI,I) :-
        sum(SetI,#=,3),
        (I #> 1
        ->
         I1 #= I-1,
         numlist(1,I1,Js),
         maplist(atmost_1_in_common_(SetI,Sets),Js)
        ;
         true
        ).
atmost_1_in_common_(SetI,Sets,J) :-
        nth1(J,Sets,SetJ),
        Common in 0..1,
        sum_union(SetI,SetJ,0,Common).

sum_union([],[],CardCommon,CardCommon).
sum_union([S1|S1s],[S2|S2s],CardCommon0,CardCommon) :-
        B in 0..1,        
        S1 + S2 #= 2 #<==> B #= 1,
        CardCommon1 #= CardCommon0 + B,
        sum_union(S1s,S2s,CardCommon1,CardCommon).        
