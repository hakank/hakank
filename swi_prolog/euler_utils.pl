/*

  Utils for Euler problems in SWI Prolog

  Here are utils for the Euler problems.
  
  This file is available at http://hakank.org/swi_prolog/euler_utils.pl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- module(euler_utils,
          [
           proc/1,
           run_problems/1,
           
           fib/2,
           fib2/2,
           even/1,
           odd/1,

           primes/2,
           delete_all/3,
           is_prime/1,
           is_prime_clp/1,
           is_prime2/1,
           prime_cp/1,
           is_prime_divisors/1,
           prime_tabled/1,
           prime_tabled_clp/1,
           next_prime/2,
           next_prime_clp/2,           
           nth_prime/2,
           nth_prime_clp/2,
           prime_decomp/2,
           divisor/2,
           divisors/2,
           all_divisors/2,
           proper_divisors/2,           
           num_divisors/2,
           sum_proper_divisors/2,
           sum_proper_divisors2/2,           
           sum_all_divisors/2,
           
           prime_divisors/2,
           prime_factors/2,
           n_factorial/2,
           factorial2/2,
           factorial3/2,
           factorial4/2,           

           palindromic/1,
           palindromic2/1,
           lcm/3,
           prodlist/2,
           mult/3,
           num_to_digit_list/2,
           digit_list_to_num/2,
           digit_list_to_num/3,           
           to_alpha/2,
           list_slices/3,
           running_prod/3,
           maplist_rev_args/3,
           concat_string_list/2,
           digits_sum/2,
           permutation_cp/3
          ]).

:- use_module(library(clpfd)).

%%
%% run_proplems(Ps)
%%
%% Run all the problems in list Ps
%%
run_problems(Ps) :-
        member(P,Ps),
        proc(P),
        fail.

run_problems(_).

%%
%% proc(P)
%%
%% Run problem P (once) and output the time.
%% It is assume that all problems writes the result.
%%
%%
proc(P) :-
        write(P),write(': '), ttyflush,
        time(once(call(P))), nl.

%%
%% fib(N,F)
%% fib2(N,F)
%%
%% F is the N'th Fibonacci number.
%% Note fib/2 is a clpfd version.
%% fib2/2 is a plain integer version which might be faster.
%%

:- table fib/2.
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

% non clpfd version
:- table fib2/2.
fib2(0, 1).
fib2(1, 1).
fib2(N,F) :-
        N >= 0,
        F > 0,
        N1 is N-1,
        N2 is N-2,
        fib(N1,F1),
        fib(N2,F2),
        F is F1+F2.

%%
%% even(N)
%% odd(N)
%%
%% N is even/odd.
%%
even(N) :- N mod 2 #= 0.
odd(N) :- N mod 2 #= 1.


%%
%% primes(N,L)
%%
%% List is a list of all primes upto N.
%%
%% Note: One might have to increase the stack size for this.
%% E.g.
%%   ?- set_prolog_stack(global, limit(10_000_000_000)).
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

%%
%% prime_cp(N)
%%
%% N is a prime, using clpfd
%%
prime_cp(N) :-
        N mod 2 #> 0,
        fd_sup(N,Max1),
        Max is round(sqrt(Max1)),
        numlist_step(3,2,Max,Is),
        maplist(not_div(N),Is).

not_div(N,Mod) :-
        N mod Mod #> 0.



is_prime_divisors(N) :-
        prime_divisors(N, Divisors),
        Divisors = [].



%%
%% Tabled versions of is_prime/2 and is_prime_clp/2
%%
:- table prime_tabled/1.
prime_tabled(P) :- is_prime(P).

%% clpfd version
:- table prime_tabled_clp/1.
prime_tabled_clp(P) :- is_prime_clp(P).

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

% clpfd verison
nth_prime_clp(Choosen, P) :-
        nth_prime_clp(1,0,Choosen, P).

%%
%% helper for nth_prime/2
%%
nth_prime(Num, Choosen, Choosen, P) :- is_prime(Num), !, P = Num.
nth_prime(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 is Ix + 1,
        next_prime(Num, P2),
        nth_prime(P2, Ix2, Choosen, P).

% clpfd version 
nth_prime_clp(Num, Choosen, Choosen, P) :- is_prime_clp(Num), P #= Num.
nth_prime_clp(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 #= Ix + 1,
        next_prime_clp(Num, P2),
        nth_prime_clp(P2, Ix2, Choosen, P).



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

%% Is D a divisor of N?
divisor(N,D) :-
        N mod D #= 0.

%% Divisors of N
%% Note: This _excludes_ 1 and N.
%%
divisors(N, Div) :-
        N2 is round(sqrt(N)),
        numlist(2,N2,Is),
        include(divisor(N),Is,Div).

%%
%% all_divisors(N,NumDivisors)
%%
%% All divisors of N (including 1 and N).
%%
all_divisors(1,[1]).
all_divisors(N,Divisors) :-
        proper_divisors(N,Divisors1),
        append(Divisors1,[N], Divisors).

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
%% sum_proper_divisors2(N,Sum)
%%
%% Sum is the sum of (proper) divisors of N (including 1 but not including N).
%%

%%
%% sum_divisors2(N,Sum)
%%
%% Sum is the sum of (proper) divisors of N (including 1 but not including N).
%%
:- table sum_proper_divisors/2.
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
:- table sum_proper_divisors2/2.
sum_proper_divisors2(N,Sum) :-
        proper_divisors(N,Div),
        sum_list(Div,Sum).


%%
%% sum_divisors(N,Sum)
%%
%% Sum is the sum of all divisors of N, including 1 and N).
%%
:- table sum_all_divisors/2.
sum_all_divisors(N,Sum) :-
        all_divisors(N,Div),
        sum_list(Div,Sum).

% Prime divisors of N
prime_divisors(N, Div) :-
        % N2 is 1+round(sqrt(N)),
        N2 is 1+round(N div 2),        
        numlist(2,N2,Is),
        include(divisor(N),Is,Div1),
        include(is_prime,Div1,Div).

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


%% clpfd version
% Prime divisors of N
prime_divisors_clp(N, Div) :-
        % N2 is 1+round(sqrt(N)),
        N2 is 1+round(N),        
        numlist(2,N2,Is),
        include(divisor(N),Is,Div1),
        include(is_prime,Div1,Div).

prime_factors_clp(N,L) :-
        N #> 0,  prime_factors_clp(N,L,2), !.

prime_factors_clp(N,L) :-
        N #> 0,
        prime_factors_clp(N,L,2).
prime_factors_clp(1,[],_) :- !.
prime_factors_clp(N,[F|L],F) :-     % N is multiple of F
        R #= N // F,
        N #= R * F, !,
        prime_factors_clp(R,L,F).
prime_factors_clp(N,L,F) :- 
        next_factor_clp(N,F,NF),
        prime_factors_clp(N,L,NF).  % N is not multiple of F


%% next_factor(N,F,NF)
%%  when calculating the prime factors of N
%%  and if F does not divide N then NF is the next larger
%%  candidate to be a factor of N.
next_factor_clp(_,2,3) :- !.
next_factor_clp(N,F,NF) :-
        F * F #< N, % !,
        NF #= F + 2.
next_factor_clp(N,_,N).


%%
%% n_factorial(N, F)
%%
%% F is the factorial of N (reversible). clpfd version.
%% 
%% From https://www.swi-prolog.org/pldoc/man?section=clpfd-factorial
%%
n_factorial(0, 1).
n_factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        F #= N * F1,
        n_factorial(N1, F1).

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
%% Using foldl/4
%%
factorial3(N,F) :-
        numlist(2,N,Is),
        foldl(mult,Is,1,F).

%%
%% Using findall/3 and prodlist/2
%%
factorial4(N,F) :-
        findall(I,
                between(2,N,I),
               Is),
        prodlist(Is,F).


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
%% lcm(X,Y,LCM)
%%
%% LCM is the lcm of X and Y.
%%
lcm(X,Y,LCM) :-
        GCD is gcd(X, Y),
        LCM is X*Y//GCD.



%%
%% prodlist(L,Prod)
%%
%% Prod is a the product of the integer in list L.
%%
prodlist(L, Prod) :-
        foldl(mult,L,1,Prod).
mult(A,B,C) :- C is A*B.


num_to_digit_list(N,L) :-
        atom_codes(N,L2),
        maplist(to_alpha,L2,L).

to_alpha(N,Alpha) :-
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
%% maplist_rev_args(Pred,Arg1,Arg2)
%%
%% Call maplist with reversed order of arguments.
%%   -> maplist(Pred,Arg2,Arg1).
%%
maplist_rev_args(Pred,Arg1,Arg2) :-
        maplist(Pred,Arg2,Arg1).


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


%%
%% digits_sum(N,Sum)
%%
%% Sum is the sum of the digits of integer N.
%%
digits_sum(N,Sum) :-
        number_chars(N,Chars),
        maplist(atom_number,Chars,Digits),
        sum_list(Digits,Sum).
        

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

