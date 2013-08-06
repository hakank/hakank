% 
% Mr Smith problem in ASP.
% 
% From an IF Prolog example (http://www.ifcomputer.de/)
% """
% The Smith family and their three children want to pay a visit but they
% do not all have the time to do so. Following are few hints who will go
% and who will not:
%     - If Mr Smith comes, his wife will come too.
%     - At least one of their two sons Matt and John will come.
%     - Either Mrs Smith or Tim will come, but not both.
%     - Either Tim and John will come, or neither will come.
%     - If Matt comes, then John and his father will
%       also come.
% """
%
% The answer should be that John and Tim will go.
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

persons(mr_smith;mrs_smith;matt;john;tim).
value(go;stay).

% unique indices of person
1 { action(P, T) : value(T) } 1 :- persons(P).

% If Mr Smith comes, his wife will come too.
action(mrs_smith, go) :- action(mr_smith, go).

% At least one of their two sons Matt and John will come.
1 { action(matt, go), action(john, go) }.

% Either Mrs Smith or Tim will come but not both.
1 { action(mrs_smith, go), action(tim, go) } 1.

% Either Tim and John will come or neither will come.
:- action(tim, T1), action(john, T2), T1 != T2.

% If Matt comes then John and his father will also come.
2 { action(john, go), action(mr_smith, go) } 2 :- action(matt, go).

#hide.
#show action(P,T).