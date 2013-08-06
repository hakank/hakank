/*

  Hamming distance in SICStus Prolog.

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance
 
  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/hamming_distance.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 6, % length of the arrays
        length(A, N),
        domain(A,0,1),
        length(B, N), 
        domain(B,0,1),

        Diffs in 0..N, % The number of differences 

        A = [1,1,1,1,1,1],
        Diffs #= 2,

        hamming_distance(A,B,Diffs),

        append(A,B,Vars1),
        append(Vars1,[Diffs],Vars),

        labeling([], Vars),
        write(diffs:Diffs),nl,
        write(a:A),nl,
        write(b:B),nl,nl,
        fail,
        fd_statistics.
%
% We can now either
% - Calculate the hamming distance from two arrays
% - Given the distance, generate all arrays which has the hamming distance
%
hamming_distance(As, Bs, Diffs) :-
        ( foreach(A,As),
          foreach(B,Bs),
          fromto(0,In,Out,Diffs) do
              Reif in 0..1,
              A #= B #<=> Reif #= 1,
              Out #= In + Reif
        ).
