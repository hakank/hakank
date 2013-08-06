/*

  toNum in ECLiPSe.

  toNum(List, Base, Num) converts a list of integers to a number for a
  base Base. It is bidirectional but it is really recommended that
  the length of List is fixed.

  See examples below.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

% :- module(tonum).
% :- export 
%    toNum/3, 
%    toNum/2,
%    exp_list/3,
%    exp_list/2.


:- lib(ic).
:- lib(ic_global).

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
        List :: [0..Base-1],
        Num #= List * Xs.


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
        ( for(I, 0, N-1), fromto([], In, Out, ExpList),  param(Base) do 
              B is Base^I,
              Out = [B|In]
        ).

%
% explist/2
%
% exponent list for base 10.
exp_list(N, ExpList) :-
        exp_list(N, 10, ExpList).


%
% Tests
% 
go :-
        length(A, 4),
        toNum(A, 10, 1234),
        writeln(a:A),

        B = [3,1,4,1,5,9],
        toNum(B, 10, Num),
        writeln(num:Num),

        length(C, 2), 
        
        % labeling of the list is needed
        findall([Num2, C], (toNum(C, 11, Num2), labeling(C)), L), 
        length(L, Len),
        writeln([Len, L]).


% % this non-CP (and unidirectional) version also works
go2 :-
        Xs = [3,2,1,0],  % len-1 .. 0
        Ys = [2,0,1,2], 
        (foreach(X, Xs), 
         foreach(Y, Ys), 
         foreach(Z, Zs) 
        do 
         Z is Y*10^X
        ), 
        Sum is sum(Zs),
        writeln([Xs, Ys, Zs, Sum]).


