/*

  Finding celebrities problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        between(1,4,P),
        writeln(problem=P),
        (
         find_celebrities(P,Celebrities)
        -> 
         length(Celebrities,N),
         findall(I,
                 (between(1,N,I),
                  element(I,Celebrities,1)
                 ),
                 Cs),
         writeln(celebrities=Cs)
        ;
         writeln(celebrities=[])
        ),
        nl,
        fail,
        nl.

go.

go2 :-
        find_celebrities(1,Celebrities),
        writeln(Celebrities),
        nl.
        

% Find the celebrities of problem P
find_celebrities(P,Celebrities) :-

        problem(P,N,Party),

        length(Celebrities,N),
        Celebrities ins 0..1,   % is this a celebrity?
        
        sum(Celebrities,#=,NumCelebrities),

        %% Celebrity:
        %% - All persons know the celebrities,
        %% - Celebrities only know other celebrities.
        numlist(1,N,Is),
        numlist(1,N,Js),        
        maplist(celebrities(Party,N,Celebrities,NumCelebrities,Js),Is),
        label(Celebrities).


/*
  foreach(I in 1..N)
  Celebrities[I] #<=> sum([(Party[J,I]#=1) : J in 1..N]) #= N,
  Celebrities[I] #<=> sum([(Party[I,J]#=1) : J in 1..N]) #= NumCelebrities
  end,
*/
celebrities(Party,N,Celebrities,NumCelebrities,Js,I) :-
        element(I,Celebrities,CelebritiesI),
        sum_c(Js,I,Party,0,Sum1,0,Sum2),
        CelebritiesI #= 1 #<==> Sum1 #= N,
        CelebritiesI #= 1 #<==> Sum2 #= NumCelebrities.

sum_c([],_I,_Party,Sum1,Sum1,Sum2,Sum2).
sum_c([J|Js],I,Party,Sum1_0,Sum1,Sum2_0,Sum2) :-
        matrix_element(Party,I,J,PartyIJ),
        matrix_element(Party,J,I,PartyJI),
        Sum1_1 #= Sum1_0 + PartyJI,
        Sum2_1 #= Sum2_0 + PartyIJ,
        sum_c(Js,I,Party,Sum1_1,Sum1,Sum2_1,Sum2).


%
% The party graph of the example above:
%
%  Adam  knows [Dan,Alice,Peter,Eva],  [2,3,4,5]
%  Dan   knows [Adam,Alice,Peter],     [1,4,5]
%  Eva   knows [Alice,Peter],     [4,5]
%  Alice knows [Peter],      [5]
%  Peter knows [Alice]       [4]
%
% Solution: Peter and Alice (4,5) are the celebrities.
%
problem(1, N, Party) :- 
        N = 5,
        Party = [
                 %1 2 3 4 5
                 [1,1,1,1,1],   % 1
                 [1,1,0,1,1],   % 2
                 [0,0,1,1,1],   % 3
                 [0,0,0,1,1],   % 4
                 [0,0,0,1,1]    % 5
                ].



% In this example Alice (4) also knows Adam (1),
% which makes Alice a non celebrity, and since
% Peter (5) knows Alices, Peter is now also a
% non celebrity. Which means that there are no
% celebrities at this party.
% 
problem(2, N, Party) :- 
   N = 5,
   Party = [
            [1,1,1,1,1],
            [1,1,0,1,1],
            [0,0,1,1,1],
            [1,0,0,1,1],
            [0,0,0,1,1]
           ].

%
% Here is another example. It has the following
% cliques:
%  [1,2]
%  [4,5,6]
%  [6,7,8]
%  [3,9,10]
%
% The celebrities are [3,9,10]
%
problem(3,N, Party) :- 
   N = 10,
   Party = [
      %   1 2 3 4 5 6 7 8 9 10
          [0,1,1,0,0,0,0,1,1,1],
          [1,0,1,0,0,0,0,0,1,1],
          [0,0,1,0,0,0,0,0,1,1],
          [0,1,1,0,1,1,0,0,1,1],
          [0,0,1,1,0,1,0,0,1,1],
          [0,0,1,1,1,0,1,1,1,1],
          [0,0,1,0,0,1,0,1,1,1],
          [0,0,1,0,0,1,1,0,1,1],
          [0,0,1,0,0,0,0,0,1,1],
          [0,0,1,0,0,0,0,0,1,1]
   ].

%
% This is the same graph as the one above
% with the following changes:
%   - 9 don't know 3 or 10
% This party graph now consists of just 
% one celebrity: [9]
%
problem(4,N,Party) :- 
   N = 10,
   Party = [
            [0,1,1,0,0,0,0,1,1,1],
            [1,0,1,0,0,0,0,0,1,1],
            [0,0,1,0,0,0,0,0,1,1],
            [0,1,1,0,1,1,0,0,1,1],
            [0,0,1,1,0,1,0,0,1,1],
            [0,0,1,1,1,0,1,1,1,1],
            [0,0,1,0,0,1,0,1,1,1],
            [0,0,1,0,0,1,1,0,1,1],
            [0,0,0,0,0,0,0,0,1,0],
            [0,0,1,0,0,0,0,0,1,1]
          ].
