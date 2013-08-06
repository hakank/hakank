%
% Dinner problem in MiniZinc.

% From http://www.sellsbrothers.com/spout/#The_Logic_of_Logic
% """
% My son came to me the other day and said, "Dad, I need help with a
% math problem." The problem went like this:
%
% * We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
% * Grandparents cost $3 for dinner, parents $2 and children $0.50
% * There must be 20 total people at dinner and it must cost $20
% * How many grandparents, parents and children are going to dinner?
% """

var 0..100: Grandparents;
var 0..100: Parents;
var 0..100: Children;

solve satisfy;

constraint
      % Grandparents * 3 + Parents * 2 + Children * 0.5 = 20 /\
      % multiply with 2:
      Grandparents * 6 + Parents * 4 + Children * 1  = 40 /\
      Grandparents + Parents + Children = 20 /\ 
       
      % at least some of each group
      Grandparents > 0 /\
      Parents      > 0 /\
      Children     > 0
;


output [
   "Grandparents: ", show(Grandparents), "\n",
   "Parents:      ", show(Parents), "\n",
   "Children:     ", show(Children), "\n"

];





