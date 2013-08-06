% 
% Set partition problem in ASP.
% 
% Problem formulation from
%   http://www.koalog.com/resources/samples/PartitionProblem.java.html
% """
%  This is a partition problem.
%  Given the set S = {1, 2, ..., n}, 
%  it consists in finding two sets A and B such that:
%  - A U B = S
%  - |A| = |B|
%  - sum(A) = sum(B)
%  - sum_squares(A) = sum_squares(B)
% """
%
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% {1, 3, 7, 8, 9, 11}, {2, 4, 5, 6, 10, 12}

#const n = 12. 

% domains
val(1..n).
partitions(1..2).

sum_domain(1..n*n #div 2).
sum_domain2(1..n*n*n #div 2).

% split the numbers in two partitions
1 { p(P, I) : partitions(P) } 1 :- val(I).

% Note that we use the same structure for all
% three conditions: cardinality, sum, sum of squares.
% Can we use that? And would it be faster?

%
% same cardinality for the partitions:
%
n #div 2 { p(P, I) : val(I) } n #div 2 :- partitions(P).
% :- S1 = #sum [p(1, I) : val(I) = 1 ], S2 = #sum [p(2, I) : val(I) = 1 ], S1 != S2.
% card(P,S) :- partitions(P), S = #sum [p(P, I) : val(I) = 1 ], S > 0.
% :- card(1,C1), card(2, C2), C1 != C2.

%
% same sums for the partitions
%
sum1(P, S) :- 
    sum_domain(S), 
    partitions(P), 
    S = #sum [p(P, I) : val(I) = I ].
:- sum1(1, S1), sum1(2, S2), S1 != S2.

%
% same sums of squares for the partitions
%
sumsq1(P, S) :- 
    sum_domain2(S), 
    partitions(P), 
    S = #sum [p(P, I) : val(I) = I*I ].
:- sumsq1(1, S1), sumsq1(2, S2), S1 != S2.


% symmetry breaking
p(1,1).

% for the output
p1(I) :- p(1, I), val(I).
p2(I) :- p(2, I), val(I).


#hide.
#show p(P, I).
#show p1(I).
#show p2(I).
% #show card(P,C).
#show sum1(P,I).
#show sumsq1(P,I).
