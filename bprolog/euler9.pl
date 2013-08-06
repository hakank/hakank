/*

  Problem 9

  http://projecteuler.net/index.php?section=problems&id=9

  A Pythagorean triplet is a set of three natural numbers, a  b  c,
  for which, a^2 + b^2 = c^2.

  For example, 32 + 42 = 9 + 16 = 25 = 52.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.

  Solution: 31875000
  31875000 ( 200**2 + 375**2 = 425**2 )

*/


% using CLP(FD)
triplet([A, B, C], Prod) :-
     LD = [A,B,C],
     LD in 1..500,
     A + B + C #= 1000,
     A #=< B, % symmetry breaking
     B #=< C, 
     Prod #= A * B *C,
     A**2 + B**2 - C**2 #= 0,
     labeling([degree,split], LD).


%
% 0.088s
%
euler9a :- 
        triplet(L, Prod),
        writeln([L,Prod]) .


is_pyth(A,B,C) :- A**2+B**2 =:= C**2.

%
% 0.12s
%
euler9b :-
        N = 1000,
        foreach(C in 1..N/2,
                (
                  foreach(B in C..-1..(N-C//2)-C,
                          [A,ABC],
                          (
                            A is N - B - C,
                            A > 0,
                            A **2 + B**2 =:= C**2,
                            is_pyth(A,B,C) ->
                            ABC is A*B*C,
                            writeln([A, B, C, ABC])
                          ;
                            true
                          )
                         )
                )
               ).




go :-
        write('euler9a: '),
        time(euler9a),
        write('euler9b: '),
        time(euler9b).
