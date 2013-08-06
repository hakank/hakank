% 
% "Global constraint" all_different except 0 in ASP.
%
% From Global constraint catalogue:
% http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
% """ 
% Enforce all variables of the collection VARIABLES to take distinct 
% values, except those variables that are assigned to 0.
% 
% Example
%    (<5, 0, 1, 9, 0, 3>)
% 
% The alldifferent_except_0 constraint holds since all the values 
% (that are different from 0) 5, 1, 9 and 3 are distinct.
% """
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 6.
#const m = 9.

values(0..m).
ix(1..n).

% unique indices of x, 1..n
1 { x(I, Val) : values(Val) } 1 :- ix(I).


%
% alldifferent except 0
% 
% If Val > 0 then there must be 0..1 
% occurrences of Val in x.
% 
{ x(I, Val) : ix(I) } 1 :- values(Val), Val > 0.


%
% Additional constraint: There must be exactly 2 zeros.
%
% 2 #sum [ x(I, 0) : ix(I) = 1 ] 2.
% Alternative:
:- not 2 #sum [ x(I, 0) : ix(I) = 1 ] 2.

#hide.
#show x(I,Val).
