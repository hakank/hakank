% 
% Prime numbers in ASP.
% 
% Unfortunately this is very slow for larger n.
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 30. % largest N
number(1..n).

%
% sqrt.
% This ensure that N is a square, which
% was not what I wanted...
%
% sqr(N, Sqrt) :- number(N;Sqrt), Sqrt*Sqrt == N.


% 
% divisor
% 
divisor(N, I) :- number(I), I > 1, I < 1+(N #div 2), N #mod I == 0, number(N), N > 1.

% divisors(N,I) :- number(N), divisor(N, I).

%
% prime number
%
% prime number if there is no number I: 2..N / 2 such that
% N % I == 0.
%
prime(N) :- number(N), N > 1, { number(I) : I > 1 : I < 1+(N #div 2) : N #mod I == 0} 0.

%
% another way of defining prime numbers:
% a number N ( > 1) is a prime number if it has no divisors
%
prime2(N) :- number(N), N > 1, {divisor(N,I) : number(I) } 0.

%
% prime divisors: divisors of N that are primes
%
prime_divisor(N, N) :- prime(N).
prime_divisor(N, I) :- number(I), I > 1, I < N, prime(I) : I < 1+(N #div 2), N #mod I == 0, number(N), N > 1.
% prime_divisor(N, I) :- divisor(N, I), prime(I).


sum_primes(Sum) :- Sum = #sum[prime(N) = N].
num_primes(Num) :- Num = #sum[prime(N) = 1].

#hide.
#show prime(N).
#show sum_primes(Sum).
#show num_primes(Num).
#show divisor(N,I).
#show prime_divisor(N,I).
#show prime2(N).