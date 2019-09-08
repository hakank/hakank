% Problem 24
% """
% A permutation is an ordered arrangement of objects. For example, 3124 is one 
% possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
% listed numerically or alphabetically, we call it lexicographic order. The 
% lexicographic permutations of 0, 1 and 2 are:
% 
%    012   021   102   120   201   210
%
% What is the millionth lexicographic permutation of the digits 
% 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
% """
%
% Answer: 2783915460
% P = [2, 7, 8, 3, 9, 1, 5, 4, 6, 0]
% (16.52s)

:- lib(listut).
:- lib(ic).
% :- lib(gfd). % Gecode

go :-
  problem24.

% Using CLP       
permut_clpfd(L, N) :-
    length(L, N),
    N1 is N-1,
    L :: 0..N1,
    ic:alldifferent(L),
    % gfd:alldifferent(L), % Gecode's
    % search(L,0,input_order,indomain_min, complete,[]). % slower: 40.94s
    % This happens to do label in lexicographic order
    labeling(L). % 16.52s

% Note: We have to increase the global stack to 1500M
%   rlwrap eclipse -g 1500M -b euler_project.ecl
% (16.8s)
problem24 :-
        writeln("Run as time eclipse -g 1500M -f euler24.ecl -e go"),
        findall(P, permut_clpfd(P,10), L),
        nth1(1000000,L,P),
        writeln(P).

