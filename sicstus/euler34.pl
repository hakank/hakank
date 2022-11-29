/*

  Euler problem 34 in SICStus Prolog

  Problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
            % euler34a,
            euler34b
            ],
        run_problems(L).

%%
%% 0.140s
%%
euler34a :-
        findall(N,
               (between(10,100000,N),
                num_to_digit_list(N,Is),
                maplist(factorial5,Is,Fs),
                sum_list(Fs,N)
               ),
               L),
        sumlist(L,Sum),
        writeln(Sum).

%%
%% 0.060s
%%
euler34b :-
        ( for(N,10,100000),
          fromto(0,In,Out,Result) do
              sum_factorial(N,S),
              (
                  N =:= S ->
                      Out is In + N
              ;
                      Out = In
              )
        ),
        writeln(Result).
              


% convert a list of ASCII integer to a list numbers
listnum(List,Num) :- 
        (foreach(L, List),
         foreach(N, Num) do
             N is L - 48
        ).


sum_factorial(N, Sum) :-
        name(N,L),
        listnum(L,List),
        ( foreach(LL,List),
          fromto(0,In,Out,Sum) do
              factorial5(LL,Fact),
              Out is In + Fact
        ).

