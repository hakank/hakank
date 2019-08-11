/*

  Magic sequence problem in SWI Prolog

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  This program implements:
  - a CLP model
  - some non-CLP "algorithmic" variants

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Magic sequence for N=10.
%%
go :-
        magic_sequence(10,Sequence),
        writeln(Sequence),
        nl.

%%
%% Test N in 4..40
%%
go2 :-
        between(4,40,N),
        (
         magic_sequence(N,Sequence)
        ->
         writeln(Sequence)
        ;
         writeln("No solution")
        ),
        nl,
        fail,
        nl.

go2.


%%
%% "Algorithmic" approach benchmark.
%%
%% Variant 1 is slightly faster than variant2.
%% The slow variant (using nth1/3) is _much_ slower.
%% Note: The CLP approach is even slower...
%%
% n:1000
% variant1: 0.000649s
% variant2: 0.000662s
% slow: 0.011812s
%
% n:10000
% variant1: 0.003742s
% variant2: 0.003768s
% slow: 0.942678s
%
% n:50000
% variant1: 0.020764s
% variant2: 0.019675s
% slow: 23.734367s
%
% n:100000
% variant1: 0.036137s
% variant2: 0.041753s
% slow: not run [about 92s]
%
% n:1000000
% variant1: 0.412016s
% variant2: 0.403703s
% slow: not run
%
% n:10000000
% variant1: 3.705814s
% variant2: 4.001884s
% slow: not run
%
go3 :-
        member(N, [1000,10_000,50_000,100_000,1_000_000,10_000_000]),
        writeln(n:N),
        
        time2(magic_sequence_no_cp(N,_Sequence1), Time1),
        format("variant1: ~fs~n", [Time1]),
        
        time2(magic_sequence_no_cp2(N,_Sequence2), Time2),
        format("variant2: ~fs~n", [Time2]),
        ( N #=< 50_000
        ->
          time2(magic_sequence_slow(N,_Sequence3), Time3),
          format("slow: ~fs~n", [Time3])
        ;
          format("slow: not run~n")
        ),
        garbage_collect,
        nl,
        fail,
        nl.

go3.


%%
%% Magic sequence. CLP approach.
%%
magic_sequence(N, Sequence) :-

        format("~n~d:~n",[N]),
        N1 #= N-1,

        length(Sequence,N),
        Sequence ins 0..N1,

        %% constraints
        sum(Sequence,#=,N),
        numlist(0,N1,Integers),
        scalar_product(Integers, Sequence, #=, N),

        %% Don't work
        % findall(I-S,(
        %              between(0,N1,I),
        %              I1 #= I+1,
        %              element(I1,Sequence,S)
        %              ),
        %         GC),
        numlist(0,N1,Is),
        gcc_ix(Is, Sequence,[], GC),
        global_cardinality(Sequence,GC),

        labeling([min,down,enum], Sequence).

%%
%% Create the pairs-list for global_cardinality/2
%%
gcc_ix([],_Sequence,GC,GC).
gcc_ix([I|Is],Sequence, GC0,[I-SI|GC]) :-
        I1 #= I+1,
        element(I1,Sequence,SI),
        gcc_ix(Is,Sequence,GC0,GC).
        

%%
%% Magic sequence, "algorithmic" approach.
%%
%%  case
%%    S[1] = N-4
%%    S[2] = 1
%%    S[3] = 1
%%    S[N-3] = 1
%%   else 
%%    S[I] = 0
%%
%%
%% Variant, slightly faster than magic_sequence_no_cp/2.
%%
magic_sequence_no_cp(N, Sequence) :-
        length(Sequence,N),
        N4 #= N-4,
        N3 #= N-3,
        Special = [1-N4, 2-2, 3-1, N3-1],
        numlist(1,N,Is),
        assign_sequence(Is,Special,[],Sequence).        

%%
%% Slightly slower than magic_sequence_no_cp/2.
%%
magic_sequence_no_cp2(N, Sequence) :-
        length(Sequence,N),
        N4 #= N-4,
        N3 #= N-3,
        Special = [1-N4, 2-2, 3-1, N3-1],
        findall(SS,
                (
                 between(1,N,I),
                 (
                  memberchk(I-S,Special)
                 ->
                  SS = S
                 ;
                  SS = 0
                 )
                 ),
                Sequence).


%%
%% Variant, very slow.
%%
magic_sequence_slow(N, Sequence) :-
        length(Sequence,N),
        N4 #= N-4,
        N3 #= N-3,
        Special = [1-N4, 2-2, 3-1, N3-1],
        numlist(1,N,Is),
        assign_sequence_slow(Is,Special,Sequence). % Much slower


%%
%% Helper for magic_sequence_no_cp/2.
%%
assign_sequence([],_Special,Sequence,Sequence).
assign_sequence([I|Is],Special,Sequence0,[T|Sequence]) :-
        (
         memberchk(I-S, Special)
        ->
         T = S
        ;
         T = 0
        ),
        assign_sequence(Is,Special,Sequence0,Sequence).

%%
%% Using nth1/3 is very slow. See timing under go3/0.
%%
assign_sequence_slow([],_Special,_Sequence).
assign_sequence_slow([I|Is],Special,Sequence) :-
        (
         memberchk(I-S, Special)
        ->
         T = S
        ;
         T = 0
        ),
        nth1(I,Sequence,T),
        assign_sequence_slow(Is,Special,Sequence).
