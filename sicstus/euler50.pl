/*

  Euler problem 50 in SICStus Prolog

  """
  The prime 41, can be written as the sum of six consecutive primes:
  41 = 2 + 3 + 5 + 7 + 11 + 13

  This is the longest sum of consecutive primes that adds to a prime 
  below one-hundred.

  The longest sum of consecutive primes below one-thousand that adds to a prime, 
  contains 21 terms, and is equal to 953.
  
  Which prime, below one-million, can be written as the sum of the most 
  consecutive primes?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler50a
             % euler50b
            ],
        run_problems(L).


%%
%% 5.505s
%%
euler50a :-
        N = 10000,
        findall(I,
                (between(1,N,I),
                 is_prime(I)
                ),
                Primes),
        e50a(550,21,Primes,0,P),
        writeln(p=P),
        nl.


e50a(found,_LimitLen,_Primes,P,P).
e50a(Len,LimitLen,Primes,P0,P) :-
        bb_put(found,false),
        (
         findall(PP,
                 (between(1,549,Offset),
                  bb_get(found,false),
                  OffsetFrom is Offset+1,
                  OffsetTo is Offset+Len,
                  findall(P,
                          (between(OffsetFrom,OffsetTo,J),
                           nth1(J,Primes,P)
                          ),
                          Ps),
                  sum_list(Ps,PP),
                  PP < 1000000,
                  is_prime(PP),
                  writeln(pp=PP),                  
                  bb_put(found,true)
                 ),
                 PPs
                ),
         PPs \= []
        ->
         PPs = [PP|_],
         e50a(found,LimitLen,Primes,PP,P)

        ;
         Len1 is Len-1,
         e50a(Len1,LimitLen,Primes,P0,P)
        ).

%%
%% Too slow.
%%
euler50b :-
        N = 10000,
        findall(I,
                (between(1,N,I),
                 is_prime(I)
                ),
                Primes),
        bb_put(found,false),
        findall(PPs,
                (between_down(550,21,Len),
                 bb_put(found,false),
                 findall(PP,
                         (between(1,549,Offset),
                          bb_get(found,false),                          
                          OffsetFrom is Offset+1,
                          OffsetTo is Offset+Len,
                          findall(P,
                                  (between(OffsetFrom,OffsetTo,J),
                                   nth1(J,Primes,P)
                                  ),
                                  Ps),
                          sum_list(Ps,PP),
                          PP < 1000000,
                          is_prime(PP),
                          bb_get(found,PP)
                         ),
                         PPs
                        )
                ),
                L),
        flatten(L,LL),
        max_list(LL,Max),
        writeln(Max).