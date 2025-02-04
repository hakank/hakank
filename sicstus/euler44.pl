/*

  Euler problem 44 in SICStus Prolog

  """  
  Pentagonal numbers are generated by the formula, P(n)=n(3n−1)/2. 
  The first ten pentagonal numbers are:

  1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

  It can be seen that P(4) + P(7) = 22 + 70 = 92 = P(8). However, 
  their difference,  70 − 22 = 48, is not pentagonal.

  Find the pair of pentagonal numbers, P(j) and P(k), for which their sum 
  and difference is pentagonal and D = |P(k) − P(j)| is minimised; what 
  is the value of D?  
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler44a
             % ,euler44b
            ],
        run_problems(L).


%%
%% 0.038s
%%
euler44a :-
        M = 2500,
        numlist(1,M,Is),
        maplist(pentagonal_number,Is,Ps),

        list_domain_disjunction(Ps,Domain),
        Vars = [J,K,A,D],
        % Vars in Domain,
        % maplist(in2(Domain),Vars),
        J in Domain,
        K in Domain,
        A in Domain,
        D in Domain,
        
        J #< K,
        A #= J+K,
        D #= abs(J-K),
        % labeling([max,bisect,down],Vars), % 1.504s
        % labeling([min,bisect,down],Vars), %  1.243s
        labeling([anti_first_fail,bisect,down],Vars), % 0.038s
        writeln(D).

in2(Domain,Var) :-
    Var in Domain.

%%
%% Too slow
%%
euler44b :-
        M = 2500,
        numlist(1,M,Is),
        maplist(pentagonal_number,Is,Ps),
        findall(B,
                (member(J,Ps),
                 member(K,Ps),
                 J < K,
                 A is J+K,
                 memberchk(A,Ps),
                 B is abs(J-K),
                 memberchk(B,Ps)
                ),
                L),
        writeln(L),
        nl.



pentagonal_number(N,P) :-
        P is N*(3*N-1) div 2.

      