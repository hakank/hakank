% https://open.kattis.com/problems/encodedmessage
% Time limit: 1s
% Diff: 1.5 Easy

:- use_module(library(clpfd)).
main :-
    read_string(_),
    repeat,
    read_string(S),
    (S == end_of_file ;
        decode(S),
        fail
    ).
main.

decode(S) :-
    string_length(S,Len),
    N is ceiling(sqrt(Len)),
    once(chunks_of(S,N,Chunks)),
    transpose(Chunks,T),
    reverse(T,TR),
    append(TR,SS),
    format('~s~n',[SS]).

read_string(S) :-
    read_line_to_codes(user_input,S).


/*
  chunks(L,N,Chunks)
  Split the list L in to chunks of length N.
  (From NFZ)
*/
chunks_of([],_N,[]).
chunks_of(L,N,[Chunk|ChunksR]) :-
    chunks_of(L,Chunk,0,N,ChunksR).

chunks_of([],[],_,_,[]).
chunks_of(L,[],N,N,Chunks) :-
    (L == [] ->
        Chunks = []
    ;
        Chunks = [NextChunk|ChunksR],
        chunks_of(L,NextChunk,0,N,ChunksR)
    ).
chunks_of([X|Xs],[X|ChunkR],Count,N,Chunks) :-
        Count1 is Count + 1,
        chunks_of(Xs,ChunkR,Count1,N,Chunks).
