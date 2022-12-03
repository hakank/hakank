/*

  Advent of Code 2022 - Day 3 in SWI Prolog

  This is the same approach as my Picat program ./3.pi (go2/0).

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

go :- 
        % File = "3_test.txt",
        File = "3.txt",
        read_file_to_string(File,Str,[]),       
        split_string(Str,"\n", "",Lines1),
        maplist(string_chars,Lines1,Lines),        
        
        %% Part 1
        maplist(part1,Lines,Scores1),
        sumlist(Scores1,Sum1),
        writeln(Sum1),

        %% Part 2
        chunks_of(Lines,3,Chunks),
        maplist(part2,Chunks,Scores2),
        sumlist(Scores2,Sum2),
        writeln(Sum2),
        nl.


part1(Line, Score) :-
        same_length(First,Second),        
        append(First,Second,Line),
        member(C,First),member(C,Second),
        alpha(Alpha),
        nth1(Score,Alpha,C).

% Altnerative part 1
part1b(Line, Score) :-
        length(Line,Len),
        Len2 is Len // 2,
        take(Line,Len2,First),
        append(First,Second,Line),
        member(C,First),member(C,Second),
        alpha(Alpha),
        nth1(Score,Alpha,C).


part2([L1,L2,L3], Score) :-
        member(C,L1),member(C,L2),member(C,L3),
        alpha(Alpha),
        nth1(Score,Alpha,C).


alpha(Chars) :-
       string_chars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",Chars).


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



/*
   take(L,N,NFirst)

   Take the first N elements from list L.
*/
take(L,N,NFirst) :-
        take(L,N,[],NFirst).
take(_,0,NFirst,NFirst) :- !.
take([H|T],N,F0,[H|F]) :-
        N > 0,
        N1 is N-1,
        take(T,N1,F0,F).

