% 
% Steiner triplets in ASP.
% 
%  http://www.probp.com/examples/clpset/steiner.pl
% """
% The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements 
% in {1,2,...,n} such that each set contains three elements and any two 
% sets have at most one element in common.
%
% For example, the following shows a solution for size n=7:
%
%     {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}
%
% Problem taken from:
% C. Gervet: Interval Propagation to Reason about Sets: Definition and 
%            Implementation of a PracticalLanguage,  
%            Constraints, An International Journal, vol.1, pp.191-246, 1997.
% """
%
% Also see:
% - http://mathworld.wolfram.com/SteinerTripleSystem.html
%   """
%   The numbers of nonisomorphic Steiner triple systems S(v) of orders 
%   v=7, 9, 13, 15, 19, ... (i.e., 6k+1,3) are 1, 1, 2, 80, 11084874829, ... 
%   """
%
% - http://en.wikipedia.org/wiki/Steiner_system
%
 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% Some statistics:
%  n    time   Choices Conflicts Restarts
%  --------------------------------------
%   7    0.01s    183     116      1
%   9    0.02s    412     136      1
%  13    0.33s   6269    2385      6
%  15    2.35s  22801   13755     10
%  19    > 2h

#const n = 7.

#const nb = n * (n-1) #div 6.

% domain
val(1..n).
sets(1..nb).


0 { x(Set, Val)  } 1 :- sets(Set), val(Val).

% 3 values of each set
3 { x(Set, Val) : val(Val) } 3 :- sets(Set).


% count the number of common occurrences of each pair of Sets
check(Set1, Set2, Val, N) :- 
    sets(Set1;Set2), 
    Set1 < Set2,
    val(Val),
    N = #count { x(Set1,Val), x(Set2,Val) }.

% ensure that there are at most 1 occurrence 
% with the same value  (i.e. N=2) for any two sets.
:- sets(Set1;Set2), Set1 < Set2, 2 #sum[check(Set1, Set2, Val, N) : N == 2 ].


%
% ad hoc symmetry breaking
% (What I would really like to do is 
% to sort the sets lexicographically.)
%
% :- sets(Set1;Set2), Set1 < Set2, 
%     S1 = #sum[x(Set1, Val1) = Val1], 
%     S2 = #sum[x(Set2, Val2) = Val2], 
%     S1 >= S2.


#hide.
#show x(Set, Val).
