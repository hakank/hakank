% background 

:- use_module(library(clpfd)).
:- use_module('/home/hakank/swi_prolog/me/hakank_utils.pl').

my_head([H|_],H).
my_tail([_|T],T).

%
% Y is X mod X
%
modulo(X,Mod,Y) :- Y #= X mod Mod.

%
% N is a square number, Sq is the square root
%
is_square(N,Sq) :- integer(N), Sq is round(sqrt(N)), N =:= Sq*Sq.

%
% latin_square/1 is defined in hakank_utils.pl
% But we have to convert the list X (1xN) to a matrix MxM
%
latin_square2(X) :-
        length(X,Len),
        is_square(Len,Square),
        chunks_of(X,Square,Xs),
        latin_square(Xs).

element3(Ix,L,Val) :- element(Ix,L,Val).

function(F) :-
        member(F,[all_different,
                  all_different_except_0,
                  %% all_distinct,
                  circuit,
                  increasing,
                  inverse
                  ]).


function_all_different(all_different).
function_alldifferent_except_0(alldifferent_except_0).
function_circuit(circuit).
function_increasing(increasing).
function_increasing_with(increasing_with).
function_inverse(inverse).


println(X) :- print(X), nl.

%
% chunks_of(L, N, Chunks)
% converts a list L to a list of lists of sizes N.
% (ported from Picat lib/util.pi)
%
chunks_of([],_N, []).
chunks_of(L,N, [Chunk|Chunks]) :-
        chunks_of(L,Chunk,0,N,Chunks).
chunks_of([],[],_,_,[]).
chunks_of(L,[],N,N,Chunks) :-
    (L == [] ->
        Chunks = []
    ;
        Chunks = [NextChunk|ChunksR],
        chunks_of(L,NextChunk,0,N,ChunksR)
    ).
chunks_of([X|Xs],[X|ChunkR],Count,N,Chunks) :-
        Count1 is Count+1,
        chunks_of(Xs,ChunkR,Count1,N,Chunks).


maplist3(M,F) :-
        maplist(M,F).

is_list([]).
is_list([_|_]).


plus(X,Y,Z) :-
        nonvar(X),nonvar(Y),
        \+ is_list(X), \+ is_list(Y),
        Z is X+Y.
mult(X,Y,Z) :-
        nonvar(X),nonvar(Y),
        \+ is_list(X), \+ is_list(Y),
        Z is X*Y.
minus(X,Y,Z) :-
        nonvar(X),nonvar(Y),
        \+ is_list(X), \+ is_list(Y),
        Z is X-Y.
divide(X,Y,Z) :-
        nonvar(X),nonvar(Y),
        \+ is_list(X), \+ is_list(Y),
        Y > 0,
        Z is X/Y.
increasing_with(L,N,L2) :-
        maplist(plus(N),L,L2).


