/*

  Finding celebrities problem in B-Prolog.

  From Uwe Hoffmann
  "Finding celebrities at a party"
  http://www.codemanic.com/papers/celebs/celebs.pdf
  """
  Problem: Given a list of people at a party and for each person the list of
  people they know at the party, we want to find the celebrities at the party. 
  A celebrity is a person that everybody at the party knows but that 
  only knows other celebrities. At least one celebrity is present at the party.
  """
  (This paper also has an implementation in Scala.)
  
  Note: The original of this problem is 
    Richard Bird and Sharon Curtis: 
    "Functional pearls: Finding celebrities: A lesson in functional programming"
    J. Funct. Program., 16(1):13–20, 2006.
  but I (as well as Hoffmann) have not been able to access this paper.

  The problem from Hoffmann's paper is to find of who are the 
  celebrity/celebrities in this party graph:
    Adam  knows {Dan,Alice,Peter,Eva},
    Dan   knows {Adam,Alice,Peter},
    Eva   knows {Alice,Peter},
    Alice knows {Peter},
    Peter knows {Alice}
  
  Solution: the celebrities are Peter and Alice.

  Note: I blogged about this problem in "Finding celebrities at a party"
  http://www.hakank.org/constraint_programming_blog/2010/01/finding_celebrities_at_a_party.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        foreach(P in 1..4, [Celebrities],
                (writeln(problem:P),
                 find_celebrities(P,Celebrities),
                 writeln(celebrities:Celebrities),
                 nl
                )).


% Find the celebrities of problem P
find_celebrities(P,Celebrities) :-

        problem(P,N,Party),

        Celebrities :: {}..{1..N},
        NumCelebrities #= #Celebrities,

        % There is at least one celebrity
        % NumCelebrities #> 0,

        % all persons know the celebrities,
        % and the celebrities only know celebrities
        % Note: We assume that a person don't know him/herself, 
        %       hence the -1 stuff.
        foreach(I in 1..N,[PartyI],
                ((I #<- Celebrities #=>
                      sum([(I #<- PartyJ): J in 1..N,
                           [PartyJ],(PartyJ @= Party[J])]) #= N-1),
                 PartyI @= Party[I],
                 (I #<- Celebrities #=> #PartyI #= (NumCelebrities-1))
                )),

        indomain(Celebrities).


%
% The party graph of the example above:
%
%  Adam  knows {Dan,Alice,Peter,Eva},  {2,3,4,5}
%  Dan   knows {Adam,Alice,Peter},     {1,4,5}
%  Eva   knows {Alice,Peter},          {4,5}
%  Alice knows {Peter},                {5}
%  Peter knows {Alice}                 {4}
%
% Solution: Peter and Alice (4,5) are the celebrities.
%
problem(1, 5,
        [
            {2,3,4,5}, % 1, Adam
            {1,4,5},   % 2, Dan 
            {4,5},     % 3, Eva
            {5},       % 4, Alice
            {4}        % 5, Peter
        ]).



% In this example Alice (4) also knows Adam (1),
% which makes Alice a non celebrity, and since
% Peter (5) knows Alices, Peter is now also a
% non celebrity. Which means that there are no
% celebrities at this party.
% 
problem(2, 5,
        [
            {2,3,4,5}, % 1, Adam
            {1,4,5},   % 2, Dan 
            {4,5},     % 3, Eva
            {1,5},     % 4, Alice
            {4}        % 5, Peter
        ]).

%
% Here is another example. It has the following
% cliques:
%  {1,2}
%  {4,5,6}
%  {6,7,8}
%  {3,9,10}
%
% The celebrities are {3,9,10}
%
problem(3,10,
        [
            {2,3,8,9,10},     % 1
            {1,3,9,10},       % 2
            {9,10},           % 3
            {2,3,5,6,9,10},   % 4
            {3,4,6,9,10},     % 5
            {3,4,5,7,8,9,10}, % 6
            {3,6,8,9,10},     % 7
            {3,6,7,9,10},     % 8
            {3,10},           % 9
            {3,9}             % 10
        ]).

%
% This is the same graph as the one above
% with the following changes:
%   - 9 don't know 3 or 10
% This party graph know consists of just 
% one celebrity: {9}
%
problem(4,10,
        [
            {2,3,8,9,10},     % 1
            {1,3,9,10},       % 2
            {9,10},           % 3
            {2,3,5,6,9,10},   % 4
            {3,4,6,9,10},     % 5
            {3,4,5,7,8,9,10}, % 6
            {3,6,8,9,10},     % 7
            {3,6,7,9,10},     % 8
            {},               % 9
            {3,9}             % 10
        ]).

