/*

  Telephone number in SWI Prolog

  http://en.wikipedia.org/wiki/Telephone_number_%28mathematics%29

  Via Spiked Math: http://spikedmath.com/545.html
  """
  ...
  Also called the involution numbers, they can be determined by the recurrence:

      a_0 = a_1 = 1;
      a_n = a_{n−1} + (n − 1) a_{n−2}, for n > 1.

  More interesting, they also count the number of involutions on a set
  with n elements -- note that an involutary function, is a function f 
  that is its own inverse, that is,

     f(f(x)) = x for all x in the domain of f. 
  """
  
  The sequence is
  1, 1, 2, 4, 10, 26, 76, 232, 764, 2620, 9496, 35696, 140152, ...
  "Number of self-inverse permutations on n letters, also known as 
   involutions; number of Young tableaux with n cells.":
  http://oeis.org/A000085


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

:- table a/2.

go :-
        between(0,23,I),
        a(I,R),
        writeln([I,R]),
        fail,
        nl.

go.

go2 :-
        abolish_all_tables,
        numlist(0,73,Is),
        make_seq(Is,[],Res),
        writeln(Res),
        nl.

make_seq([],L,L).
make_seq([I|Is],L0,[R|L]) :-
        a(I,R),
        make_seq(Is,L0,L).

a(0,1).
a(1,1).
a(N,Res) :-
        N #>= 0,
        N1 #= N-1,
        N2 #= N-2,
        a(N1,N1Res),
        a(N2,N2Res),
        Res #= N1Res + N1* N2Res.

