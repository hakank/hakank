%
% Subset sum in ASP.
%
% From Katta G. Murty: "Optimization Models for Decision Making", page 340
% http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
% 
% """
% Example 7.8.1
% 
% A bank van had several bags of coins, each containing either
% 16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
% street, thieves stole some bags. A total of 100 coins were lost.
% It is required to find how many bags were stolen.
% """
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 6.
#const total = 100.
#const max_bags = 10.

% The values of each bag
bag(1, 16).
bag(2, 17).
bag(3, 23).
bag(4, 24).
bag(5, 39).
bag(6, 40).

% domain
bags(1..n).

amount(0..max_bags).

% unique index:
1 { taken(Bag, Amount) : amount(Amount) } 1 :- bags(Bag).

% sum of the stolen bags are 100
100 #sum [taken(Bag, Amount) : amount(Amount) : bag(Bag, Val) = Val*Amount ] 100.
