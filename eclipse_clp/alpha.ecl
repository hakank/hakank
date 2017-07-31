/*

  Alpha puzzle (a.k.a. crypto) in ECLiPSe.

  This is just an alternative to the example
  in SICTus' lib/clpfd/examples/alpha.pl .

  Compare with the following models:
  * ECLiPSe : http://www.hakank.org/eclipse/crypto.ecl
  * MiniZinc: http://www.hakank.org/minizinc/crypto.mzn
  * Comet   : http://www.hakank.org/comet/crypto.co
  * SICStus Prolog : http://www.hakank.org/sicstus/alpha.pl

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
        alpha(LD,Backtracks),
        writeln(LD),
        writeln(backtracks:Backtracks).


alpha(LD, Backtracks) :-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
	LD :: 1..26,
	alldifferent(LD),

	p([B,A,L,L,E,T],        45),
	p([C,E,L,L,O],          43),
	p([C,O,N,C,E,R,T],      74),
	p([F,L,U,T,E],          30),
	p([F,U,G,U,E],          50),
	p([G,L,E,E],            66),
	p([J,A,Z,Z],            58),
	p([L,Y,R,E],            47),
	p([O,B,O,E],            53),
	p([O,P,E,R,A],          65),
	p([P,O,L,K,A],          59),
	p([Q,U,A,R,T,E,T],      50),
	p([S,A,X,O,P,H,O,N,E], 134),
	p([S,C,A,L,E],          51),
	p([S,O,L,O],            37),
	p([S,O,N,G],            61),
	p([S,O,P,R,A,N,O],      82),
	p([T,H,E,M,E],          72),
	p([V,I,O,L,I,N],       100),
	p([W,A,L,T,Z],          34),
        
        search(LD,0,first_fail,indomain_min,complete,
               [backtrack(Backtracks)]).

p(Xs, Sum) :-
        Sum #= sum(Xs).
