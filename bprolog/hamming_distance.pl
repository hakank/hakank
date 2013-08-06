/*

  Hamming distance in B-Prolog.

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

% 
% We can now either
% - Calculate the hamming distance from two arrays
% - Given the distance, generate all arrays which has the 
%   Hamming distance.
% 
go :-
        N = 6, % length of the arrays
        length(A, N),
        A :: 0..1,
        length(B, N), 
        B :: 0..1,

        Diffs :: 0..N, % The number of differences 
        % Diffs #= 2,
        indomain(Diffs),


        A = [1,1,1,1,1,1],

        hamming_distance(A,B,Diffs),

        term_variables([A,B,Diffs],Vars),
        labeling(Vars),

        writeln(diffs:Diffs),
        writeln(a:A),
        writeln(b:B),nl,
        fail.


hamming_distance(As, Bs, Diffs) :-
        Diffs #= sum([(A #\= B) : (A,B) in (As,Bs)]).
