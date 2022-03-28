/*

  Logic puzzle in SWI Prolog

  clpfd version of the puzzle from
  https://swi-prolog.discourse.group/t/similar-einstein-riddle/5142
  """
  Friends: Jarek, Frank, Stefanie, Albert, Simon, Robert, Marcin
  they live in one building, each on a different level (0, 1, 2, …).
  Each of them has a different animal:
  dog, a cat, a fish, hamster, parrot, snake, canary.
  In which floort each of them lives and what kind of animal does it have when bellow sentences are true?

  Franek lives lower than Albert. The owner of the parrot lives on floor 6. The canary owner lives on floor 0.
  Franek has no snake. The snake owner lives on floor 1. Albert doesn’t have a canary.
  Jarek doesn’t have a parrot. Simon has no snake. Robert has no hamsters. The hamster owner lives on floor 3.
  Robert lives lower than Stefan. Szymon lives lower than Marcin. Marcin has no fish.
  The owner of the fish lives on floor 4. Franek doesn’t have a hamster. Albert lives lower than Stefan.
  The cat owner lives on floor 5. The dog owner lives on floor 2. Szymon lives lower than Jarek.
  Robert lives higher than Albert. Albert doesn’t have a dog. Franek doesn’t have a dog.
  Stefan doesn’t have a canary. Franek doesn’t have a cat. Marcin lives higher than Jarek.
  Stefan doesn’t have a parrot. Franek lives lower than Marcin. Marcin doesn’t have a snake.
  Marcin lives higher than Albert. Franek has no fish. Szymon lives higher than Stefan. Robert has no fish.
  Stefan lives higher than Franek. Simon lives higher than Albert. Robert doesn’t have a cat.
  Marcin lives higher than Robert. Franek lives lower than Szymon. Franek lives lower than Robert.
  Stefan lives lower than Jarek. Stefan doesn’t have a cat. Jarek lives higher than Franek.
  Stefan doesn’t have a dog. Jarek lives higher than Robert. Franek doesn’t have a parrot. Stefan has no snake.
  Albert has no fish. Robert lives lower than Simon. Jarek lives higher than Albert. Albert doesn’t have a cat.
  Stefan lives lower than Marcin. Albert doesn’t have a parrot. Stefan has no fish. Simon doesn’t have a parrot.
  Simon doesn’t have a cat. Albert doesn’t have a hamster. Jarek doesn’t have a dog. Simon doesn’t have a hamster.
  Marcin doesn’t have a dog. Simon doesn’t have a canary. Simon doesn’t have a dog.
  """

  Solution:
  floor=[1,0,5,6,2,4,3]
  owns=[2,1,6,7,3,5,4]
  Floor 0 Frank canary
  Floor 1 Albert snake
  Floor 2 Robert dog
  Floor 3 Stefanie hamster
  Floor 4 Simon fish
  Floor 5 Jarek cat
  Floor 6 Marcin parrot

  This model was in part inspired by joeblog's solution.

  Cf my Picat model: http://hakank.org/picat/friends_at_different_floors.pi
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).

go :- 
        puzzle(Floor,Owns),
        writeln(floor=Floor),
        writeln(owns=Owns),        

        Animals = [canary,snake,dog,hamster,fish,cat,parrot],
        People = ['Albert','Frank','Jarek','Marcin','Robert','Simon','Stefanie'], % sorted
        numlist(0,6,FloorNums),
        maplist(print_solution(Animals,People,Floor),FloorNums),        
        nl,
        fail,

        nl.
go.


print_solution(Animals,People,Floor, FloorNum) :-
        % Who lives at this floor?
        nth1(P,Floor,FloorNum), % Identify this person's index
        nth1(P,People,Person),  % Get the name
        % What animal is on this floor?
        nth0(FloorNum,Animals,Animal),
        format("Floor ~d ~w ~w\n", [FloorNum,Person,Animal]).

puzzle(Floor,Owns) :-
        N = 7,
        numlist(1,N,Ns),

        % The animals at the different floors
        Animals = Ns,
        % Floor order
        [Canary,Snake,Dog,Hamster,Fish,Cat,Parrot] = Animals, 

        % The people
        People = Ns,  
        [Albert,Frank,Jarek,Marcin,Robert,Simon,Stefanie] = People, 

        % What animal does person P owns?
        length(Owns,N),
        Owns ins 1..N,

        % Which Floor does person P live at?
        N1 is N-1,
        length(Floor,N),
        Floor ins 0..N1,

        all_different(Owns),
        all_different(Floor),

        % [Person, NotOwns]
        NoOwns = [[Frank,    [Snake, Dog, Hamster, Fish, Cat, Parrot]],
                  [Albert,   [Canary, Dog, Hamster, Fish, Cat, Parrot]],
                  [Robert,   [Hamster, Fish, Cat]],
                  [Stefanie, [Canary, Snake, Dog, Fish, Cat, Parrot]],
                  [Simon,    [Canary, Snake, Dog, Hamster, Cat, Parrot]],
                  [Jarek,    [Dog, Parrot]],
                  [Marcin,   [Snake, Dog, Fish]]
                 ],
        maplist(no_owns(Owns),NoOwns),

        % All constraints are converted to lower than (inspired by joeblog's solution)
        % [Floor[Person] < [People]]
        LowerFloors =  [
                   [Frank,    [Albert,Robert,Stefanie,Simon,Jarek,Marcin]],
                   [Albert,   [Robert,Stefanie,Simon,Jarek,Marcin]],
                   [Robert,   [Stefanie,Simon,Jarek,Marcin]],
                   [Stefanie, [Simon,Jarek,Marcin]],
                   [Simon,    [Jarek,Marcin]],
                   [Jarek,    [Marcin]]
                  ],
        maplist(lower_floors(Floor),LowerFloors),
        
        append(Owns,Floor,Vars),
        label(Vars).

%
% Person P doesn't own any animal in the list As.
% 
no_owns(Owns,[P,As]) :-
        maplist(no_own(Owns,P),As).

% Person P does not own animal A
no_own(Owns,P,A) :-
        element(P,Owns,PO),
        PO #\= A.

%
% Person P lives in a floor less than all people in L.
% 
lower_floors(Floor,[P,Ps]) :-
        maplist(lower_floor(Floor,P),Ps).
% Person P live in a floor less than P2.
lower_floor(Floor,P,P2) :-
        element(P,Floor,FloorP),
        element(P2,Floor,FloorP2),
        FloorP #< FloorP2.
