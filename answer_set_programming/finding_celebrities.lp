% 
% Finding celebrities problem in ASP.
% 
% From Uwe Hoffmann
% "Finding celebrities at a party"
% http://www.codemanic.com/papers/celebs/celebs.pdf
% """
% Problem: Given a list of people at a party and for each person the list of
% people they know at the party, we want to find the celebrities at the party. 
% A celebrity is a person that everybody at the party knows but that 
% only knows other celebrities. At least one celebrity is present at the party.
% """
% (This paper also has an implementation in Scala.)
% 
% Note: The original of this problem is 
%   Richard Bird and Sharon Curtis: 
%   "Functional pearls: Finding celebrities: A lesson in functional programming"
%   J. Funct. Program., 16(1):13–20, 2006.
% but I (as well as Hoffmann) have not been able to access this paper.
%
% The problem from Hoffmann's paper is to find of who are the 
% celebrity/celebrities in this party graph:
%   Adam  knows {Dan,Alice,Peter,Eva},
%   Dan   knows {Adam,Alice,Peter},
%   Eva   knows {Alice,Peter},
%   Alice knows {Peter},
%   Peter knows {Alice}
% 
% Solution: the celebrities are Peter and Alice.
%
% Note2: I blogged about this problem in 
% "Finding celebrities at a party"
% http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

knows(adam,  dan;alice;peter;eva).
knows(dan,   adam;alice;peter).
knows(eva,   alice;peter).
knows(alice, peter).
knows(peter, alice).

% extended problem instance:
% here hakan, anna, and agatha are the celebrities.
% 
% knows(adam,  dan;alice;peter;eva;hakan;anna;agatha).
% knows(dan,   adam;alice;peter;hakan;anna;agatha).
% knows(eva,   alice;peter;adam;hakan;anna;agatha).
% knows(alice, peter;adam;hakan;anna;agatha).
% knows(peter, alice;adam;hakan;anna;agatha).
% knows(hakan, anna;agatha).
% knows(anna,  hakan;agatha).
% knows(agatha,  hakan;anna).

% extract some info
person(X) :- knows(X, _).
num_p(N) :- N = #sum [person(P) ].

% 1) a person is a celebrity if everyone
% knows P
celebrity(C) :- 
    person(C),
    num_p(N),
    N-1 { knows(P, C) : person(P) } N-1.

% 2) and the celebrities only know other
% celebrities, i.e.
% C is not a celebrity if he/she
% knows anyone that is not a celebrity)
:- celebrity(C), person(C), not celebrity(P), knows(C, P).
% variant:
% { celebrity(C) } 0 :-     
%     person(C),
%     not celebrity(P),
%     knows(C, P).



#hide.
#show celebrity(C).
