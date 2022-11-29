/*

  Euler problem 39 in SICStus Prolog

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
            euler39a
            ],
        run_problems(L).

%%
%% 3.579s
%%
euler39a :-
        N = 1000,
        findall(I2,
                (between(1,N,I),
                 I2 is I*I
                ),
                Squares),
        findall(C,
                (member(A,Squares),
                 member(B,Squares),
                 A =< B,
                 AB is A+B,
                 memberchk(AB,Squares),
                 C is round(sqrt(A) + sqrt(B) + sqrt(AB)),
                 C =< 1000
                ),
                Valid),
        sort(Valid,Sorted),
        findall([Count-C],
                (member(C,Sorted),
                 % count_occurrences(Valid,C,Count) % slightly slower 3.5s
                 count_occ(Valid,C,Count)
                ),
                L),
        sort(L,Counts1),
        reverse(Counts1,Counts),
        Counts = [[_MaxCount-Num]|_],
        writeln(Num).

