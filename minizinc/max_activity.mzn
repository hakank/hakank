% 
% Max activity problem in MiniZinc.
% 
% From
% Need help for defining appropriate constraints
% http://stackoverflow.com/questions/25806906/need-help-for-defining-appropriate-constraints
% """
% I'm very new to constraint programming and try to find some real situations to 
% test it. I found one i think may be solved with CP.
% 
% Here it is : I have a group of kids that i have to assign to some activities. 
% These kids fill a form where they specify 3 choices in order of preference. Activities 
% have a max number of participant so, the idea is to find a solution where the choices 
% are respected for the best without exceedind max.
% 
% So, in first approach, i defined vars for kids with [1,2,3] for domain (the link 
% between the number of choice, activity and children being known somewhere else).
% 
% But then, i don't really know how to define relevant constraints so I have all the 
% permutation (very long) and then, i have to give a note to each (adding the numbers of 
% choices to get the min) and eliminate results with to big groups.
% 
% I think there must be a good way to do this using CP but i can't figure it out.
% 
% Does someone can help me ?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_kids;
array[1..num_kids, 1..3] of int: prefs;

int: num_activities;
array[1..num_activities] of int: activity_size;

% decision variables
array[1..num_kids] of var 1..num_activities: x; % the selected activity
array[1..num_kids] of var 1..num_activities: scores;

var int: total_score = sum(scores);

solve maximize total_score;
% solve satisfy;
% solve :: int_search(x ++ scores, first_fail, indomain_split, complete) maximize total_score;

constraint
  forall(k in 1..num_kids) (
    % select one of the prefered activities

    % exists(p in 1..3) (
    %   x[k] = prefs[k,p] /\
    %   scores[k] = 4-p % score for the selected activity
    % )
    % alternative:
    let {
      var 1..3: p
    } in 
      x[k] = prefs[k,p] /\
      scores[k] = 4-p % score for the selected activity
  )

  /\ % ensure size of the activities
  % 
  % forall(a in 1..num_activities) (
  %  sum([bool2int(x[k] = a) | k in 1..num_kids]) <= activity_size[a]
  % )

  global_cardinality_low_up(x, [i | i in 1..num_activities], [0 | i in 1..num_activities], activity_size)

  % /\ total_score = 17 % for solve satisfy and the second activity_size

;

output [
  "x     : ", show(x), "\n",
  "scores: ", show(scores), "\n",
  "total_score: ", show(total_score), "\n",

];

%
% some small fake data
%
num_kids = 6;
num_activities = 4;

% Activity preferences for each kid
prefs = array2d(1..num_kids, 1..3,
[
  1,2,3,
  4,2,1,
  2,1,4,
  4,2,1,
  3,2,4,
  4,1,3
]);

% max size of activity
activity_size = [2,2,2,3];
% activity_size = [2,2,2,2];