% 
% Young tableaux in ASP.
% 
% See 
% http://mathworld.wolfram.com/YoungTableau.html
% and
% http://en.wikipedia.org/wiki/Young_tableau
% """
% The partitions of 4 are
%  {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
%
% And the corresponding standard Young tableaux are:
%
% 1.   1 2 3 4
%
% 2.   1 2 3         1 2 4    1 3 4
%      4             3        2
%
% 3.   1 2           1 3
%      3 4           2 4
%
% 4    1 2           1 3      1 4 
%      3             2        2 
%      4             4        3
%
% 5.   1
%      2
%      3
%      4
% """  
% 

% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n=4.

%
% domains
%
one_n(1..n).
one_nplus(1..n+1).
zero_nplus(0..n+1).


%
% The empty cells in x are represented as the number n+1 
% for simplifying the comparisons. 
%

% unique indices:
1 { x(I, J, V) : one_nplus(V) } 1 :- one_n(I;J).
1 { p(I, V) : zero_nplus(V) } 1 :- one_n(I).


% 1..n is used exactly once in x
1 { x(I, J, V) : one_n(I;J) } 1 :- one_n(V).

% always start with 1 in upper left corner
:- x(1,1,V), V != 1.

% row wise
:- x(I,J,V1), x(I,J2,V2), J2 > J, V2 < V1.

% column wise
:- x(I,J,V1), x(I2,J,V2), I2 > I, V2 < V1.

% calculate the structure of the partition:
% p(I) is the sum of "real" values (<=n) in the I'th row of x
:- p(I, Val), Sum = #sum[ x(I,J, V) : one_n(J) : V <= n ], Val != Sum.

% decreasing order of p
:- p(I-1, V1), p(I, V2), V1 < V2.

% sum(p) = n
:- P = #sum[p(Ix, Val) = Val], P != n.


% output of x where all values V>n are
% removed.
x_out(I,J, V) :- x(I,J,V), V <= n.

% output of p where all values V = 0 removed.
% p_out(I,V) :- p(I,V), V > 0.

#hide.
% #show x(I,J, V).
#show p(I, V).
#show x_out(I, J, V).
% #show p_out(I, V).


