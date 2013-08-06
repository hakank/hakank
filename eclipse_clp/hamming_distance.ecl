/*

  Hamming distance in ECLiPSe.

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance
 
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/hamming_distance.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/hamming_distance.pl



  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).



go :-
        N = 6, % length of the arrays
        length(A, N),
        A :: 0..1,
        length(B, N), 
        B :: 0..1,

        Diffs :: 0..N, % The number of differences 

        A = [1,1,1,1,1,1],
        Diffs #= 2,

        hamming_distance(A,B,Diffs),

        term_variables([A,B,Diffs],Vars),

        search(Vars,0,first_fail,indomain_min,complete,[]),
        writeln(diffs:Diffs),
        writeln(a:A),
        writeln(b:B),nl,
        fail.
%
% We can now either
% - Calculate the hamming distance from two arrays
% - Given the distance, generate all arrays which has the hamming distance
%
hamming_distance(As, Bs, Diffs) :-
        ( foreach(A,As),
          foreach(B,Bs),
          fromto(0,In,Out,Diffs) do
              Reif #= (A #\= B),
              Out #= In + Reif
        ).
