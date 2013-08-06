/*

  Alpha puzzle (a.k.a. crypto) in SICStus Prolog.

  This is just an alternative to the example
  in SICTus' lib/clpfd/examples/alpha.pl .

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/crypto.mzn
  * Comet   : http://www.hakank.org/comet/crypto.co
  * ECLiPSe : http://www.hakank.org/eclipse/crypto.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        alpha([ff,bisect], value, LD),
        write(LD), nl,
        fd_statistics.


alpha(Lab, Consistency, LD) :-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
	Opt = [consistency(Consistency)],
	domain(LD,1,26),
	all_different(LD, Opt),

	p([B,A,L,L,E,T],        45, Opt),
	p([C,E,L,L,O],          43, Opt),
	p([C,O,N,C,E,R,T],      74, Opt),
	p([F,L,U,T,E],          30, Opt),
	p([F,U,G,U,E],          50, Opt),
	p([G,L,E,E],            66, Opt),
	p([J,A,Z,Z],            58, Opt),
	p([L,Y,R,E],            47, Opt),
	p([O,B,O,E],            53, Opt),
	p([O,P,E,R,A],          65, Opt),
	p([P,O,L,K,A],          59, Opt),
	p([Q,U,A,R,T,E,T],      50, Opt),
	p([S,A,X,O,P,H,O,N,E], 134, Opt),
	p([S,C,A,L,E],          51, Opt),
	p([S,O,L,O],            37, Opt),
	p([S,O,N,G],            61, Opt),
	p([S,O,P,R,A,N,O],      82, Opt),
	p([T,H,E,M,E],          72, Opt),
	p([V,I,O,L,I,N],       100, Opt),
	p([W,A,L,T,Z],          34, Opt),
        
	labeling(Lab,LD).

%
% Build the 1's in Sx of the same length as Xs.
%
p(Xs, Sum, Opt) :-
        (
          foreach(X,Xs),
          fromto(Sx, [1|In], In, [])
        do true
        ),
        scalar_product(Sx, Xs, #=, Sum, Opt).
