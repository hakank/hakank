/*

  Some utilities in SICStus Prolog.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)). % for between/3 and numlist/3
:- use_module(library(mutdict)).


time(Goal) :-
  statistics(runtime,[Start|_]),
  call(Goal),
  statistics(runtime,[Stop|_]),
  RuntimeSec is Stop - Start,
  Runtime is RuntimeSec / 1000,
  format("Time: ~3fs~n",[Runtime]).


% Portability of SWI Prolog
writeln(V) :-
        write(V), nl.


% Portability of SWI Prolog
sum_list(L,Sum) :-
        sumlist(L,Sum).

max_list(L,Max) :-
    max_member(Max,L).

min_list(L,Min) :-
    min_member(Min,L).


%%
%% run_proplems(Ps)
%%
%% Run all the problems in list Ps
%%
run_problems(Ps) :-
        member(P,Ps),
        once(proc(P)),
        fail.

run_problems(_).

%% 
%% filter(Predicate,List1,List2)
%%
filter(P, List1, List2) :-
        ( foreach(X,List1), 
          fromto(List2,Out,In,[]), 
          param(P) do 
              applyP(P, X) -> Out = [X|In] ; Out=In
        ).

applyP(P, Xs) :- Query =.. [P,Xs], Query.


%%
%% proc(P)
%%
%% Run problem P (once) and output the time.
%% It is assume that all problems writes the result.
%%
%%
proc(P) :-
        write(P),write(': '),
        flush_output,
        time(once(call(P))), nl.


%%
%% fib(N,F)
%% fib2(N,F)
%%
%% F is the N'th Fibonacci number.
%% Note fib/2 is a clpfd version.
%% fib2/2 is a plain integer version which might be faster.
%%
fib(0, 1).
fib(1, 1).
fib(N,F) :-
        N #>= 0,
        F #> 0,
        N1 #= N-1,
        N2 #= N-2,
        fib(N1,F1),
        fib(N2,F2),
        F #= F1+F2.

%% non clpfd version 
%% SICStus does not support tabling
%% :- table fib2/2. 
fib2(0, 1).
fib2(1, 1).
fib2(N,F) :-
        N >= 0,
        F > 0,
        N1 is N-1,
        N2 is N-2,
        fib2(N1,F1),
        fib2(N2,F2),
        F is F1+F2.

%%
%% fib3(Cache,N,F)
%% A cached version of fib.
%%
fib3(_Dict,0, 1).
fib3(_Dict,1, 1).
fib3(Dict,N,F) :-
        N > 1,        
        (mutdict_get(Dict,N,FMemo) -> 
         F = FMemo
        ; 
         % F > 0,
         N1 is N-1,
         N2 is N-2,
         fib3(Dict,N1,F1),
         fib3(Dict,N2,F2),
         F is F1+F2,
         mutdict_put(Dict,N,F)
        ).

fib_memo(0, 1).
fib_memo(1, 1).
fib_memo(N,F) :-
        N >= 0,
        N1 is N-1,
        N2 is N-2,
        memo(fib_memo(N1,F1)),
        memo(fib_memo(N2,F2)),
        F is F1+F2.


% From https://www.metalevel.at/prolog/memoization
:- dynamic memo_/1.
memo(Goal) :-
        (   memo_(Goal)
        ->  true
        ;   once(Goal),
            assertz(memo_(Goal))
        ).
    


%%
%% even(N)
%% odd(N)
%%
%% N is even/odd.
%%
even(N) :- N mod 2 #= 0.
odd(N) :- N mod 2 #= 1.



prime_factors(N,L) :-
        N > 0,  prime_factors(N,L,2), !.

prime_factors(N,L) :-
        N > 0,
        prime_factors(N,L,2).
prime_factors(1,[],_) :- !.
prime_factors(N,[F|L],F) :-     % N is multiple of F
        R is N // F,
        N is R * F, !,
        prime_factors(R,L,F).
prime_factors(N,L,F) :- 
        next_factor(N,F,NF),
        prime_factors(N,L,NF).  % N is not multiple of F


%% next_factor(N,F,NF)
%%  when calculating the prime factors of N
%%  and if F does not divide N then NF is the next larger
%%  candidate to be a factor of N.
next_factor(_,2,3) :- !.
next_factor(N,F,NF) :-
        F * F < N, % !,
        NF is F + 2.
next_factor(N,_,N).


%%
%% is_prime(N)
%% is_prime_clp(N)
%%
%% True if N is a prime number
%%
is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  


% CLPFD version
is_prime_clp(2).
is_prime_clp(3).
is_prime_clp(P) :- integer(P), P #> 3, P mod 2 #\= 0, \+ has_factor(P,3).  


%%
%% is_prime2(N)
%%
%% True if N is a prime number
%%
is_prime2(2) :- !.
is_prime2(3) :- !.
is_prime2(X) :-
        X > 3,
        X mod 2 =\= 0,
        is_prime2_(X, 3).
is_prime2_(X, N) :-
        ( N*N > X
        ->
          true
        ;
          X mod N =\= 0,
          M is N + 2,
          is_prime2_(X, M)
        ).


is_prime3(2).
is_prime3(3).
is_prime3(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor3(P,3).  

has_factor3(N,L) :- N mod L =:= 0.
has_factor3(N,L) :- L * L < N, L2 is L + 2, has_factor3(N,L2).


%%
%% prime_cp(N)
%%
%% N is a prime, using clpfd
%%
prime_cp(N) :-
        N mod 2 #> 0,
        fd_max(N,Max1),
        Max is round(sqrt(Max1)),
        numlist_step(3,2,Max,Is),
        maplist(not_div(N),Is).

not_div(N,Mod) :-
        N mod Mod #> 0.



is_prime_divisors(N) :-
        prime_divisors(N, Divisors),
        Divisors = [].


%
% From http://rosettacode.org/wiki/Prime_decomposition#Prolog
%
prime_decomp(N, L) :-
	SN is round(sqrt(N)),
	prime_decomp_1(N, SN, 2, [], L).
 
prime_decomp_1(1, _, _, L, L) :- !.

% Special case for 2, increment 1
prime_decomp_1(N, SN, D, L, LF) :-
	(   0 #= N mod D ->
	    Q #= N // D,
	    SQ is round(sqrt(Q)),
	    prime_decomp_1(Q, SQ, D, [D |L], LF)
	;
	    D1 #= D+1,
	    (	D1 #> SN ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).
 
% General case, increment 2
prime_decomp_2(1, _, _, L, L) :- !.
prime_decomp_2(N, SN, D, L, LF) :-
	(   0 #= N mod D ->
	    Q #= N // D,
	    SQ is integer(floor(sqrt(Q))),
	    prime_decomp_2(Q, SQ, D, [D |L], LF);
	    D1 #= D+2,
	    (	D1 #> SN
            ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).



%%
%% Tabled versions of is_prime/2 and is_prime_clp/2
%%
prime_tabled(Dict,P) :-
    (mutdict_get(Dict,P,Val) ->
        Val == true
    ; 
        (is_prime(P) ->
            mutdict_put(Dict,P,true),
            true
        ;
            mutdict_put(Dict,P,false),
            false
        )
    ).

%% clpfd version
prime_tabled_clp(Dict,P) :- 
    (mutdict_get(Dict,P,Val) ->
        Val == true
    ; 
        (is_prime_clp(P) ->
            mutdict_put(Dict,P,true),
            true
        ;
            mutdict_put(Dict,P,false),
            false
        )
    ).


%%
%% has_factor(N,L)
%% has_factor_clp(N,L)
%%
%% True if N has the factor L.
%%
has_factor(N,L) :- N mod L =:= 0, !.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

% clpfd version
has_factor_clp(N,L) :- N mod L #= 0.
has_factor_clp(N,L) :- L * L #< N, L2 #= L + 2, has_factor_clp(N,L2).



%%
%% next_prime(Num,P)
%% next_prime_clp(Num,P)
%%
%% P is the next prime after N
%% 
next_prime(Num, P) :- Num2 is Num + 1, next_prime2(Num2, P).
next_prime2(Num, P) :- is_prime(Num), !, P = Num.
next_prime2(Num, P) :-
        Num2 is Num+1,
        next_prime2(Num2,P).

% clpfd verison
next_prime_clp(Num, P) :- Num2 is Num + 1, next_prime2_clp(Num2, P).
next_prime2_clp(Num, P) :- is_prime_clp(Num), !, P #= Num.
next_prime2_clp(Num, P) :-
        Num2 #= Num+1,
        next_prime2_clp(Num2,P).

%%
%% nth_prime(N,P)
%% nth_prime_clp(N,P)
%%
%% P is the nth prime
%%
nth_prime(N, P) :-
        nth_prime(1,0,N, P).
%%
%% helper for nth_prime/2
%%
nth_prime(Num, Choosen, Choosen, P) :- is_prime(Num), !, P = Num.
nth_prime(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 is Ix + 1,
        next_prime(Num, P2),
        nth_prime(P2, Ix2, Choosen, P).

% clpfd verison
nth_prime_clp(Choosen, P) :-
        nth_prime_clp(1,0,Choosen, P).

% clpfd version 
nth_prime_clp(Num, Choosen, Choosen, P) :- is_prime_clp(Num), P #= Num.
nth_prime_clp(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 #= Ix + 1,
        next_prime_clp(Num, P2),
        nth_prime_clp(P2, Ix2, Choosen, P).


%%
%% primes(N,L)
%%
%% List is a list of all primes upto N.
%%
primes(N,L) :-
        N2 is 1+(N div 2),
        findall(J2,(between(2,N2,I),
                    II is I*I,
                    II =< N,
                    NDivI is 1+(N div I),
                    between(0,NDivI,J),
                    J2 is II+I*J,
                    J2 =< N
                   ),
                Js),
        sort(Js,Deletes),        
        numlist(2,N,All),
        %% subtract(All,Deletes,L).
        delete_all(All,Deletes,L).
    
    
primes2(N,L) :-
        N2 #= 1+(N div 2),
        findall(J2,(between(2,N2,I),
                    II #= I*I,
                    II #=< N,
                    NDivI #= 1+(N div I),
                    between(0,NDivI,J),
                    J2 #= II+I*J,
                    J2 #=< N
                   ),
                Js),
        sort(Js,Deletes),        
        numlist(2,N,All),
        delete_all(All,Deletes,L).



%%
%% delete_all(L,Deletes,L2)
%%
%% L2 is a list with all elements in L which
%% are not in the list Deletes.
%%
delete_all(L,Deletes,L2) :-
        delete_all(L,Deletes,[],L2).

delete_all([],_Deletes,L2,L2) :- !.
%% we might have some X left even though he Deletes list is empty.
delete_all(X,[],L2,[X|L2]).
delete_all([X|Xs],[X|Deletes],L0,L) :-
        delete_all(Xs,Deletes,L0,L).
delete_all([X|Xs],[D|Deletes],L0,[X|L]) :-
        X \= D,
        delete_all(Xs,[D|Deletes],L0,L).


%%
%% Divisors
%%
divisors(N, Divisors2) :-
        X is integer(1+(N // 2)), 
        ( for(I,1,X), 
          fromto(Divisors,Out,In,[]),
          param(N) do 
              N mod I =:= 0 -> 
              Out = [I|In] 
        ; 
              Out = In
        ),
        append(Divisors,[N], Divisors2).

%
% Here we just take the divisors
% from 2..sqrt(N) and then add the rest (N mod Divisors).
% For some N the last element in Divisors1 is the same as
% first element in Divisors2 which we must handle.
% This happens when N is a square.
%
divisors2(N, Divisors) :-
        N1 is N+1,
        X is integer(floor(sqrt(N1))), 

        % get the first half of the divisors
        ( for(I,1,X), 
          fromto(Divisors1,Out1,In1,[]),
          param(N) do 
              N mod I =:= 0 -> 
              Out1 = [I|In1]
        ; 
              Out1 = In1
        ),
        % now make the second half
        ( foreach(D1,Divisors1),
          foreach(D2,Divisors2),
          param(N) do
              D2 is N // D1
        ),
        % When N is a square we must remove the first element in 
        % the reversed list (since it's the same as the last in the
        % first list)
        reverse(Divisors2,Divisors2b),
        last_element(Divisors1, Last1),
        Divisors2b = [First2b|Divisors2c],
        (
            Last1 \= First2b ->
                append(Divisors1,Divisors2b,Divisors)
        ;
                append(Divisors1,Divisors2c,Divisors)
        ).   

last_element(List,Last) :-
        length(List,Len),
        nth1(Len,List,Last).


%
% The N'th triangle number is n*(n+1) / 2
%
% (for(I,1,10), foreach(T,Triangle) do triangle(I,T)).
%
% I = I
% T = T
% Triangle = [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
triangle(N, T) :- T is N *(N+1) // 2.


%%
%% num_divisors(N,NumDivisors)
%%
%% NumDivisors is the number of divisors of N.
%%
%% Method:
%% - Get all the (prime) factors of N
%% - For each factor: get the exponent (number of occurrences) and add 1
%% - Multiply these counts to get the number of divisors.
%%
%% Example: num_divisors(144,15)
%%
%% Step:
%%
%%  ?- prime_factors(144,D).
%%  D = [2, 2, 2, 2, 3, 3].
%%
%%  144 = 2^4 + 3^2
%%  The exponents are 4 and 2, respectively.
%%  The number of divisors is thus: (4+1) + (2+1) = 5 * 3 = 15
%%
%% Also, see https://primes.utm.edu/glossary/page.php?sort=Tau
%%
num_divisors(N,NumDivisors) :-
        prime_factors(N,Factors),
        sort(Factors,Sorted), %% The distinct factors
        %% get the number of occurrences of each factor
        findall(Count,
                (member(M,Sorted),
                 findall(M,
                         member(M,Factors),
                         Ms),
                 length(Ms,MsLen),
                 %% add 1 to the number of this factor
                 Count is MsLen+1
                ),
                Counts),
        prodlist(Counts,NumDivisors).

%%
%% proper_divisors(N,NumDivisors)
%%
%% All divisors of N, including 1 (but not N).
%%
proper_divisors(1,[1]).
proper_divisors(N,Divisors) :-
        N > 1,
        N2 is N div 2,
        findall(I,
                (between(1,N2,I),
                 N mod I =:= 0
                ),
                Divisors).


%%
%% sum_proper_divisors2(N,Sum)
%%
%% Sum is the sum of (proper) divisors of N (including 1 but not including N).
%%

%%
%% sum_divisors2(N,Sum)
%%
%% Sum is the sum of (proper) divisors of N (including 1 but not including N).
%%
sum_proper_divisors(N,Sum) :-
        sum_proper_divisors(2,N,1,Sum), !.

sum_proper_divisors(I,N,Sum,Sum) :-
        I > floor(sqrt(N)).

% I is a divisor of N
sum_proper_divisors(I,N,Sum0,Sum) :-
        N mod I =:= 0,
        NdivI is N div I,
        Sum1 is Sum0 + I,
        (I \= NdivI
        -> 
         Sum2 is Sum1 + NdivI
        ; 
         Sum2 is Sum1
        ),
        I1 is I+1,
        sum_proper_divisors(I1,N,Sum2,Sum).

% I is no divisor of N.
sum_proper_divisors(I,N,Sum0,Sum) :-
        % N mod I \= 0,
        I1 is I+1,
        sum_proper_divisors(I1,N,Sum0,Sum).

%% Slower variant
% :- table sum_proper_divisors2/2.
sum_proper_divisors2(N,Sum) :-
        proper_divisors(N,Div),
        sum_list(Div,Sum).


%%
%% palindromic(L)
%%
%% True if list L is palindromic.
%%
palindromic(L) :-
        reverse(L,L).

%%
%% palindromic2(L)
%%
%% True if integer N is palindromic.
%%
palindromic2(N) :-
        number_codes(N,L),
        reverse(L,L).


%%
%% prodlist(L,Prod)
%%
%% Prod is a the product of the integer in list L.
%%
prodlist(L, Prod) :-
        foldr(mult,1,L,Prod).
mult(A,B,C) :- C is A*B.


%
% recursive version
%
prodlist2(List,Prod) :-
  prodlist2_aux(List,1,Prod).

prodlist2_aux([], Prod,Prod).
prodlist2_aux([H|T],Prod0,Prod) :-
   Prod1 is H*Prod0,
   prodlist2_aux(T,Prod1,Prod).

% Convert a number to a list of numbers
num_to_digit_list(N,L) :-
    name(N,L2),
    maplist(to_alpha,L2,L).

to_alpha(N,Alpha) :-
        Alpha is N - 48.


to_alpha_cp(N,Alpha) :-
        Alpha #= N - 48.


%%
%% digit_list_to_num(L,N)
%%
%% N is the integer in base Base given the digits in list L.
%%
digit_list_to_num(L,N) :-
        digit_list_to_num(L,10,N).
digit_list_to_num(L,Base,N) :-
        length(L,Len),
        reverse(L,Rev),
        findall(J,
                (between_down(Len,1,I),
                 nth1(I,Rev,D),
                 J is D*Base^(I-1)
                ),
                Bases),
        sum_list(Bases,N).

%%
%% digits_sum(N,Sum)
%%
%% Sum is the sum of the digits of integer N.
%%
digits_sum(N,Sum) :-
    number_codes(N,Codes),
    maplist(code_digit,Codes,Digits),
    sum_list(Digits,Sum).

code_digit(Code,Digit) :-
    Digit is Code-48.

%%
%% list_slice(L, SliceLen, Slices)
%%
%% slices a list L into slices of length SliceLen
%% and returns a list Slice
%%
list_slices(L, SliceLen, Slices) :-
        length(L,Len),
        N #= Len-SliceLen+1,
        findall(Slice,
                (between(1,N,I),
                 To #= I+SliceLen-1,
                 findall(S,
                         (
                          between(I,To,J),
                          nth1(J,L,S)
                         ),
                         Slice)
                ),
                Slices).

%%
%% running_prod(RLen, L1, L2)
%%
%% L2 is a list of running product of each length RLen slices of L1.
%% 
%% Note: RLen is the first argument to simplify using maplist, e.g.
%%      maplist(running_prod(RLen), L1,L2)
%%
running_prod(RLen, L1, L2) :-
        list_slices(L1, RLen, Slices),
        maplist(prodlist,Slices,L2).

%%
%% foldr(Op,Init,List,Result)
%%
foldr(_Op, Init, [], Init).
foldr(Op, Init, [X|Xs], R) :- 
        foldr(Op, Init, Xs, R1), 
        P =.. [Op, X, R1, R], 
        call(P).

%%
%% lcm(X,Y,LCM)
%%
%% LCM is the lcm of X and Y.
%%
lcm(X,Y,LCM) :-
        GCD is gcd(X, Y),
        LCM is X*Y//GCD.



%%
%% n_factorial(N, F)
%%
%% (Note: This is a port of the clpfd version in SWI Prolog but
%%  SICStus clpfd module cannot handle very large integers,
%%  so it's plain integer version. And it's not reversible)
%% 
%% From https://www.swi-prolog.org/pldoc/man?section=clpfd-factorial
%%
n_factorial(0, 1).
n_factorial(N, F) :-
        N > 0,
        N1 is N - 1,
        n_factorial(N1, F1),
        F is N * F1.

%%
%% Plain recursive.
%%
factorial2(N,F) :-
        factorial2(N,1,F).
factorial2(0,F,F).
factorial2(N,F0,F) :-
        F1 is F0*N,
        N1 is N-1,
        factorial2(N1,F1,F).

%%
%% Using foldr/4
%%
factorial3(N,F) :-
        numlist(2,N,Is),
        foldr(mult,1,Is,F).

%%
%% Using findall/3 and prodlist/2
%%
factorial4(N,F) :-
        findall(I,
                between(2,N,I),
               Is),
        prodlist(Is,F).



factorial5(N,Factorial) :-
        ( for(I,1,N),
          fromto(1,In,Out,Factorial) do
              Out is In * I
        ).


%%
%% between_down(From, To, N)
%%
%% Count down from From to To (From >= N).
%%
between_down(N, M, K) :-
        N #>= M,
        K #= N.
between_down(N, M, K) :-
        N #> M,
        N1 #= N-1,
        between_down(N1, M, K).



% between with steps
between(From,Step,To,N) :-
    between(From,To,Tmp),
    % Tmp =< To,
    N is (Tmp-From)*Step+From,
    N =< To.

%%
%% numlist_step(L,Step,U,Ls)
%%
%% Ls is a list of L..Step..U.
%%
%% As numlist/3 but with step Step.
%%
%% Examples:
%%   ?- numlist_step(1,2,10,L).
%%   [1,3,5,7,9]
%%%  ?- numlist_step(1,2,11,L)
%%   [1,3,5,7,9,11]
%%
numlist_step(L, Step, U, Ns) :-
        numlist_step_(L, Step, U, Ns).

numlist_step_(L, Step, U, [L]) :-
        L + Step > U,
        !.
numlist_step_(L, Step, U, [U]) :-
        L + Step =:= U,
        !.
numlist_step_(L, Step, U, [L|Ns]) :-
        L+Step =< U,        
        L2 is L+Step,
        numlist_step_(L2, Step, U, Ns).



%%
%% matrix_element(X, I, J, Val)
%%
%% Matrix[I,J] = Val
%%
matrix_element(X, I, J, Val) :-
        matrix_element2(X,I,J,Val).

%%
%% Different approaches.
%% matrix_element2/4 and matrix_element5/4 seems to work best.
%%
% matrix_element1(X, I, J, Val) :-
%         element(I, X, Row),
%         element(J, Row, Val).

matrix_element2(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).

% matrix_element3(X, I, J, Val) :-
%         freeze(I, (nth1(I, X, Row),freeze(J,nth1(J,Row,Val)))).

% matrix_element4(X, I, J, Val) :-
%         freeze(I, (element(I, X, Row),freeze(J,element(J,Row,Val)))).

matrix_element5(X, I, J, Val) :-
        nth1(I, X, Row),
        nth1(J, Row, Val).

%%
%% matrix_nth1(X, I, J, Val)
%%
%% A better name for matrix_element5/3.
%%
matrix_nth1(X, I, J, Val) :-
        nth1(I, X, Row),
        nth1(J, Row, Val).

%%
%% element2(I,X,Y)
%%
%% Y[X[I]] #= I (symmetry between two lists),
%% cf. inverse/2.
%% 
element2(I,X,Y) :-
   element(I,X,XI),
   element(XI,Y,XIY),
   XIY #= I.

%%
%% Reverse number_codes/2 for maplist/3.
%%
codes_number(String,Number) :-
        number_codes(Number,String).


%%
%% maplist_rev_args(Pred,Arg1,Arg2)
%%
%% Call maplist with reversed order of arguments.
%%   -> maplist(Pred,Arg2,Arg1).
%%
maplist_rev_args(Pred,Arg1,Arg2) :-
        maplist(Pred,Arg2,Arg1).


read_file(Filename, Words) :-
        open(Filename,read,Stream),
        read_list(Stream, Words),
        close(Stream).

read_list(Stream,LL) :-
    read_line(Stream,Line),
    Line \== end_of_file,    
    LL = [Line|L],
    !,
    read_list(Stream,L).
read_list(_,[]).


%%
%% concat_string(ListOfString,Str)
%%
%% Str is a concatenation of the strings in ListOfString.
%%
concat_string_list(L,Str) :-
        concat_string_list(L,"",Str).
concat_string_list([],Str,Str).
concat_string_list([S|L],Str0,Str) :-
        string_concat(Str0,S,Str1),
        concat_string_list(L,Str1,Str).

string_concat(S1,S2,S) :-
    append(S1,S2,S).




%
% http://en.wikipedia.org/wiki/Julian_day
% gregorian date -> julian day
date2julian(Year,Month,Day, JD) :-
  A is floor((14-Month) / 12), % 1 for Jan or Feb, 0 for other months
  Y is Year + 4800 - A,
  M is Month + 12*A - 3, % 0 for Mars, 11 for Feb
  JD is Day + floor( (153*M + 2) / 5) + 365*Y + floor(Y/4) -
       floor(Y / 100) + floor(Y / 400) - 32045.


% julian day -> gregorian date
julian2date(JD, Date) :-
  Y is 4716,
  V is 3,
  J is 1401,
  U is 5,
  M is 2,
  S is 153,
  N is 12,
  W is 2,
  R is 4,
  B is 274277,
  P is 1461,
  C is  -38,
  F is JD + J + (((4 * JD + B) div 146097) * 3) div 4 + C,
  E is R * F + V,
  G is mod(E, P) div R,
  H is U * G + W,
  Day is (mod(H, S)) div U + 1,
  Month is mod(H div S + M, N) + 1,
  Year is (E div P) - Y + (N + M - Month) div N,
  Date = [Year,Month,Day].


%%
%% split_string(String,Separator,Lines)
%%
split_string(Str,Sep,Lines) :-
    split_string(Str,Sep,[],[],Lines).

split_string([],_Sep,Cur,Lines,Lines2) :-
    append(Lines,[Cur],Lines2).
split_string([Sep|Str],Sep,Cur,Lines0,Lines) :-
    append(Lines0,[Cur],Lines1),
    split_string(Str,Sep,[],Lines1,Lines).

split_string([C|Str],Sep,Cur,Lines0,Lines) :-
    C \= Sep,
    append(Cur,[C],Cur1),
    split_string(Str,Sep,Cur1,Lines0,Lines).



%%
%% list_domain_disjunction([D|Ds],Conj)
%%
%% Convert a list of integers to a disjunction of domain
%% which in SICStus Prolog is represented as
%%   
%%| ?-  write_canonical({1}\/{2}\/{3}\/{4}\/{5}\/{6}).
%% \/(\/(\/(\/(\/({}(1),{}(2)),{}(3)),{}(4)),{}(5)),{}(6))
%%
%% | ?- list_domain_disjunction([1,10,21,100],Disj).
%% Disj = {1}\/{100}\/{21}\/{10} ? ;
%%
list_domain_disjunction([D|Ds],Disj) :-
        foldr(disj,{D},Ds,Disj).
disj(A,B,C) :-C = \/(B,{A}).



% 
% toNum/3
% toNum(-List, +Base, -Num)
%
% Converts a list of integers in base Base to a number Num, or
% vice versa.
% 
% This is a bidirectional predicate, but if the length of List 
% is not bounded there is an infinite solutions with 0 added 
% to the head of the list for each solution.
% 
% Examples:
% 
%   toNum([1,2,3,4], 10, Num).      -> Num = 1234
%
%   L is of bounded length 
%   length(L,3), toNum(L, 10, 123). -> L = [1,2,3]
%   length(L,3), toNum(L, 10, 12).  -> L = [0,1,2]
% 
%   another base:
%   toNum([1,2,3], 12, Num).        -> Num =  171
%
%   L is of unbounded length:
%   toNum(L, 10, 12).  -> L = [1,2], L = [0,1,2], L = [0,0,1,2], ....
%
toNum(List, Base, Num) :-
        length(List, Len),
        length(Xs, Len),
        exp_list(Len, Base, Xs), % calculate exponents
        Base1 is Base-1,
        domain(List, 0, Base1),
        scalar_product(Xs,List, #=, Num).



%
% Defaults to Base 10
%
toNum(List, Num) :-
        toNum(List, 10, Num).
   
%
% exp_list/3
% exp_list(+N, +Base, -ExpList)
% 
% ExpList is a list of the exponents 
%       [Base^(N-1), Base^(N-2),...,Base^0],
% 
% Example:
%   exp_list(3, 10, ExpList). -> ExpList = [100,10,1]
%
exp_list(N, Base, ExpList) :-
        (
            for(I, 0, N-1),
            fromto([], In, Out, ExpList),
            param(Base)
        do 
            B is integer(Base**I),
            Out = [B|In]
        ).
%
% explist/2
%
% exponent list for base 10.
exp_list(N, ExpList) :-
        exp_list(N, 10, ExpList).


to_num(List,Base,Num) :-
    toNum(List,Base,Num).

to_num(List,Num) :-
    toNum(List,10,Num).
              

%
% Flatten a list of lists.
%
flatten(Ls,Flatten) :-
    append(Ls,Flatten).


%%
%% rotate(L,L2)
%%
%% rotate a list (put first element -> last)
%%
rotate(L,L2) :-
        L=[X|LRest], append(LRest,[X],L2).



%%
%% dec_to_base_list(N,Base,L)
%%
%% Convert decimal integer N to a list L of digits in base Base.
%%
dec_to_base_list(N,Base,L) :-
        dec_to_base_list(N,Base,[],L).

dec_to_base_list(0,_Base,L,L).
dec_to_base_list(N,Base,L0,[R|L]) :-
        N > 0,
        R is N mod Base,
        N1 is N div Base,
        dec_to_base_list(N1,Base,L0,L).


%
% A non CP variant of count_occurrences
%
count_occ(L,Element,Count) :-
    count_occ(L,Element,0,Count).

count_occ([],_Element,Count,Count).
count_occ([H|T],Element,Count0,Count) :-
    (H == Element ->
        Count1 is Count0 + 1
    ;
        Count1 is Count0
    ),
    count_occ(T,Element,Count1,Count).
    
%%
%% count_occurrences(L,Element,Count)
%%
%% In the list L, there must be exactly Count occurrences of Element
%%
count_occurrences(L,Element,Count) :-
        count_occurrences_(L,Element,0, Count).

count_occurrences_([],_Element,Count, Count).
count_occurrences_([H|T],Element,Count0, Count) :-
        B in 0..1,
        H #= Element #<=> B #= 1,
        Count1 #= Count0 + B,
        count_occurrences_(T,Element,Count1, Count).


%%
%% permutation_cp(A,B,Js)
%%
%% List B is a permutaion of list A with the permutation
%% indices Js (restricted to 1..length(A)).
%%
permutation_cp(A,B,Js) :-
        length(A,Len),
        numlist(1,Len,Is),
        maplist(permutation_cp2(A,B),Is,Js).

permutation_cp2(A,B,I,J) :-
        element(I,A,AI),
        element(J,B,AI).

