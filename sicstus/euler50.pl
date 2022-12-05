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
            % euler50a % ,
            %% euler50b
            % euler50c
            euler50d
            ],
        run_problems(L).


%%
%% 5.431s
%%
euler50a :-
        N = 10000,
        primes(N,Primes),
        e50a(550,21,Primes,0,P),
        writeln(P),
        nl.


e50a(found,_LimitLen,_Primes,P,P).
e50a(Len,LimitLen,Primes,P0,P) :-
    bb_put(found,false),
    findall(PP,
             (between(1,549,Offset),
              bb_get(found,false),
              OffsetFrom is Offset+1,
              OffsetTo is Offset+Len,
              OffsetTo =< 10000,
              findall(P,
                      (between(OffsetFrom,OffsetTo,J),
                       nth1(J,Primes,P)
                      ),
                      Ps),
              sumlist(Ps,PP),
              PP < 1000000,
              is_prime(PP),
              bb_put(found,true)
             ),
             PPs
           ),
    (PPs \= [] ->
        PPs = [PP|_],
        e50a(found,LimitLen,Primes,PP,P)
        
    ;
        Len1 is Len-1,
        e50a(Len1,LimitLen,Primes,P0,P)
    ).

%%
%% Too slow.
%%
/*
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
                          sumlist(Ps,PP),
                          PP < 1000000,
                          writeln(pp=PP),
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
*/


euler50c :-
        N = 10000,
        primes(N,Primes),
        e50c(550,21,Primes,0,P),
        writeln(P),
        nl.

e50c(found,_LimitLen,_Primes,P,P).
e50c(Len,LimitLen,Primes,P0,P) :-
    Len \= found,
    Len >= LimitLen,
    (for(Offset,1,549),
     fromto(PPs,Out,In,[]),
     param(Len,Primes,P0,P,LimitLen) do     
     OffsetFrom is Offset+1,
     OffsetTo is Offset+Len,
     OffsetTo =< 10000,
     % Sum the primes in the range OffsetFrom..Offset
     (for(J,OffsetFrom,OffsetTo),
      fromto(0,In2,Out2,PP),
      param(Primes) do
      nth1(J,Primes,PJ),
      Out2 is In2 + PJ
     ),
     ((PP < 1000000,
       is_prime3(PP)) ->
         Out = [PP|In]
     ;
         Out = In
     )
    ),
    (PPs \= [] ->
        writeln(len=Len),
        sort(PPs,FoundPrimes1),
        reverse(FoundPrimes1,FoundPrimes),
        head(FoundPrimes,MaxPrime),
        e50c(found,LimitLen,Primes,MaxPrime,P)
    ;
        Len1 is Len-1,
        e50c(Len1,LimitLen,Primes,P0,P)
    ).


%%
%% CP approach: 1.625s
%%
euler50d :-
    N = 3950, % 10000,
    primes(N,Primes),
    length(Primes,PLen),

    length(X,PLen),
    domain(X,0,1),

    Start in 1..PLen,
    End in 1..PLen,

    Start #< End,
    (for(I,1,PLen),
     param(X,Start,End) do
     element(I,X,XI),
     I #< Start #=> XI #= 0,
     I #> End #=> XI #= 0,
     (I #>= Start #/\ I #=< End) #<=> XI #= 1
    ),
    scalar_product(Primes,X,#=,P),
    P #< 1000000,
    prime_cp(P),
    
    sum(X,#=,S),
    S #>= 21,
    
    append(X,[S,P,Start,End],Vars),
    labeling([maximize(S),ff,bisect,down], Vars),
    writeln(s=S),
    writeln(P).

:- dynamic is_prime/2.
is_prime_memo(N) :-
    (is_prime(N,Val) ->
        Val == true
    ;
        (is_prime(N) ->            
            assertz(is_prime(N,true))
        ;
            assertz(is_prime(N,false)),
            false
        )
    ).
