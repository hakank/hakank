% 
% Bus scheduling in ASP.
% 
% Scheduling minimum number of buses.
% 
% Problem from Taha "Introduction to Operations Research", page 58.
%
% This is a slightly more general model than Taha's.
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 6. % number of time slots

% demand(time t, minimum buses at time t)
demand(1,8).
demand(2,10).
demand(3,7).
demand(4,12).
demand(5,4).
demand(6,4).

% domains
times(1..n).
val(0..12).
sum_val(1..24).

1 { x(Time, N) : val(N) } 1 :- times(Time).

%
% sum "this" time with the following time, for times 1..n-1
%
sum_two(Time, N1+N2) :- x(Time, N1), x(Time+1 , N2).
:- #sum[ sum_two(Time, Sum) : sum_val(Sum) = Sum ] Demand-1, demand(Time, Demand), Time < n.

%
% sum "around the corner" (times n + 1)
%
:- demand(n, Demand), x(n, Nn), x(1, N1), Nn+N1 < Demand.

%
% Minimize the total number of buses
%
#minimize [ x(Time, N) = N ].


#hide.
#show x(Time, N).