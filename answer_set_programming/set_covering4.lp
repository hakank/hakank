%
% Set partition, set covering in ASP.
% 
% Example from Lundgren, Ronnqvist, Varbrand "Optimeringslara", page 408.
% 
% We want to minimize the cost of the alternatives which covers all the 
% objects, i.e. all objects must be choosen. The requirement is than an object 
% may be selected _exactly_ once.
%
% Alternative        Cost        Object
% 1                  19           1,6
% 2                  16           2,6,8
% 3                  18           1,4,7
% 4                  13           2,3,5
% 5                  15           2,5
% 6                  19           2,3
% 7                  15           2,3,4
% 8                  17           4,5,8
% 9                  16           3,6,8
% 10                 15           1,6,7
%
% The problem has a unique solution of z = 49 where alternatives 
% 3, 5, and 9 is selected. 
%
% If we, however, allow that an object is selected more than one time, 
% then the solution is z = 45 (i.e. less cost than the first problem),
% and the alternatives 4, 8, and 10 is selected, where object 5 is 
% selected twice (alt. 4 and 8). It's an unique solution as well.
% 
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%


#const num_alternatives = 10.
#const num_objects = 8.

alternative(1, 1;6).
alternative(2, 2;6;8).
alternative(3, 1;4;7).
alternative(4, 2;3;5).
alternative(5, 2;5).
alternative(6, 2;3).
alternative(7, 2;3;4).
alternative(8, 4;5;8).
alternative(9, 3;6;8).
alternative(10, 1;6;7).

cost(1, 19).
cost(2, 16).
cost(3, 18).
cost(4, 13).
cost(5, 15).
cost(6, 19).
cost(7, 15).
cost(8, 17).
cost(9, 16).
cost(10, 15).

%
% domains
%
alternatives(1..num_alternatives).
objects(1..num_objects).

%
% set partition: all objects must be selected by _exactly_ one alternative 
%
% 1 { selected(Alternative) : alternatives(Alternative) : alternative(Alternative, Object) } 1 :- objects(Object).

%
% set covering: all objects must be selected by _at least_ one alternative 
%
1 { selected(Alternative) : alternatives(Alternative) : alternative(Alternative, Object) } :- objects(Object).

%
% minimize the cost of the selected alternatives
%
#minimize [selected(Alternative) : alternatives(Alternative) : cost(Alternative, Cost) = Cost ].