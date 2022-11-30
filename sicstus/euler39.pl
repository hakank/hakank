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
            % euler39a,
            euler39b
            ],
        run_problems(L).

%%
%% 0.972s
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
                 C is round(sqrt(A) + sqrt(B) + sqrt(AB)),
                 C =< 1000,
                 memberchk(AB,Squares)
                ),
                Valid),
        findall([Count-C],
                (member(C,Valid),
                 % count_occurrences(Valid,C,Count) % slightly slower 
                 count_occ(Valid,C,Count)
                ),
                L),
        sort(L,Counts),
        last(Counts,[_MaxCount-Num]),
        writeln(Num).

%%
%% Using for-loops.
%% 0.590s
%%
euler39b :-
        N = 1000,
        (for(I,1,N),
         fromto(Squares,[I2|In],In,[]) do
           I2 is I*I
        ),
        (foreach(A,Squares),
         foreach(Valid1,Valid2),
         param(Squares) do
         (foreach(B,Squares),
          fromto(Valid1,Out,In,[]),
          param(A,Squares) do          
          ((AB is A+B,C is round(sqrt(A) + sqrt(B) + sqrt(AB)),A =< B,
            C =< 1000,
            memberchk(AB,Squares)
              ) ->
               Out = [C|In]
           ;
               Out = In
           )
         )
        ),
        flatten(Valid2,Valid),
        (foreach(CC,Valid),
         fromto(L,[[Count-CC]|In],In,[]), param(Valid) do
           count_occ(Valid,CC,Count)
        ),
        sort(L,Counts),
        last(Counts,[_MaxCount-Num]),
        writeln(Num).

