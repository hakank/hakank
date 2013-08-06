%
% Another set_covering problem in ASP.
% 
% Problem from 
% Katta G. Murty: "Optimization Models for Decision Making", page 302f
% http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
% 
% 10 senators making a committee, where there must at least be one 
% representative from each group:
% group:        senators:
% southern      1 2 3 4 5
% northern      6 7 8 9 10
% liberals      2 3 8 9 10
% conservative  1 5 6 7
% democrats     3 4 5 6 7 9
% republicans   1 2 8 10
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% 5 answers:
%  $  clingo --opt-all set_covering3.lp
%  selected(6) selected(2) 
%  selected(7) selected(2) 
%  selected(8) selected(5) 
%  selected(9) selected(1) 
%  selected(10) selected(5) 
%


#const num_groups = 6.
#const num_senators = 10.

% what group does a senator belong to
%  1  2  3  4  5  6  7  8  9 10
%  ---------------------------------
%  1, 1, 1, 1, 1, 0, 0, 0, 0, 0,   % 1 southern
%  0, 0, 0, 0, 0, 1, 1, 1, 1, 1,   % 2 northern
%  0, 1, 1, 0, 0, 0, 0, 1, 1, 1,   % 3 liberals
%  1, 0, 0, 0, 1, 1, 1, 0, 0, 0,   % 4 conservative
%  0, 0, 1, 1, 1, 1, 1, 0, 1, 0,   % 5 democrats
%  1, 1, 0, 0, 0, 0, 0, 1, 0, 1    % 6 republicans

senator(1, southern;conservative;republicans).
senator(2, southern;liberals;republicans).
senator(3, southern;liberals;democrats).
senator(4, southern;democrats).
senator(5, southern;conservative;democrats).
senator(6, northern;conservative;democrats).
senator(7, northern;conservative;democrats).
senator(8, northern;liberals;republicans).
senator(9, northern;liberals;democrats).
senator(10, northern;liberals;republicans).



senators(1..num_senators).
groups(southern;northern;liberals;conservative;democrats;republicans).

% All groups must be represented by at least one selected senators.
1 { selected(Senator) : senators(Senator) : senator(Senator, Group) } :- groups(Group).

% minimize the number of senators
#minimize [selected(Senator) : senators(Senator) ].

#hide.
#show selected(Senator).