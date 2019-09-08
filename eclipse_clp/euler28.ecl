%
% Problem 28
%
% """
% Starting with the number 1 and moving to the right in a clockwise 
% direction a 5 by 5 spiral is formed as follows:
% 
%    21 22 23 24 25
%    20  7  8  9 10
%    19  6  1  2 11
%    18  5  4  3 12
%    17 16 15 14 13
%
% It can be verified that the sum of the numbers on the diagonals is 101.
% 
% What is the sum of the numbers on the diagonals in a 1001 by 1001 
% spiral formed in the same way?
% """
%
% Answer: 669171001 (0.00s)
%
% (0.0s)

go :-
  problem28,
  problem28b.

problem28 :-
        ( for(N,3,1001,2),
          fromto(1,In,Out,Sum) do
              Out is In + 4*N^2 - 6*N + 6
        ),
        writeln(Sum).
     
% (0.0s)   
problem28b :-
        findall(Res, 
                (between(3,1001,2,N), Res is 4*N^2-6*N+6),
                L), 
        sum(L,Sum),
        Sum1 is Sum+1,
        writeln(Sum1).
