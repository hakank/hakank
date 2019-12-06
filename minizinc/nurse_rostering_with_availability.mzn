% 
% Nurse rostering with availability in MiniZinc.
% 
% From Reconciling Scheduled shifts with availabilities
% http://stackoverflow.com/questions/22238698/reconciling-scheduled-shifts-with-availabilities
% """
% I'm writing a program to help schedule student employees at our university based on 
% pre-defined shifts (time blocks) and student availabilities (also time blocks).
% 
% This strikes me as similar to the Air Crew problem, except for modelling the constraint 
% that an employee is not simply generally available: it depends on their schedule.
% 
% What modelling strategies can a more seasoned constraint programmer recommend for 
% solving this problem?
% 
% (I'm using Gecode.)
% """

% Here I just use the Nurse rostering model (from the MiniZinc examples) and 
% add a (random) availability matrix.
% 
% My answer to the question: http://stackoverflow.com/a/22267889/195636
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_nurses = 7;
int: num_days = 14;

int: day_shift = 1;
int: night_shift = 2;
int: off_shift = 3;

% the DFA (for regular)
int: n_states = 6;
int: input_max = 3;
int: initial_state = 1;
set of int: accepting_states = 1..6;

array[1..3] of string: days = ["d","n","o"];

% d: days, n: nights, o:off
array[1..n_states, 1..input_max] of int: transition_fn =
 array2d(1..n_states, 1..input_max,
  [
      % d,n,o
        2,3,1,
        4,4,1,
        4,5,1, 
        6,6,1,
        6,0,1,
        0,0,1
  ]);

%
% Availability matrix (for the 7 x 14 problem instance)
% Note: for this schedule there can be max 2 non-available
% nurses per day.
%
array[1..num_nurses, 1..num_days] of int: available = 
  array2d(1..num_nurses, 1..num_days,
    [
    % days
      1,1,0,1,1,1,1, 1,1,1,0,1,1,1, % nurses
      0,1,1,1,0,1,1, 0,1,1,1,1,1,1, 
      1,0,1,1,0,1,1, 1,0,0,1,1,0,1, 
      1,1,1,1,1,0,1, 1,0,1,1,1,1,0, 
      1,1,1,0,1,1,1, 1,1,1,1,0,1,1, 
      1,1,0,1,1,0,1, 1,1,0,1,1,1,0, 
      1,1,1,0,1,1,1, 1,1,1,0,1,1,1, 
    ]
  );


array[1..num_nurses, 1..num_days] of var day_shift..off_shift: x;

% summary of the shifts
array[1..num_days, 1..3] of var 0..num_nurses: stat;

% solve satisfy;
solve :: int_search (
        [ x[i,j] | i in 1..num_nurses, j in 1..num_days]
        % ++ [stat[j,t] | j in 1..num_days, t in 1..3]
        , 
        first_fail, % max_regret, 
        indomain_split, 
        complete) 
    satisfy;

constraint
   forall(i in 1..num_nurses) (
       regular([x[i,j] | j in 1..num_days], n_states, input_max, transition_fn,
                   initial_state, accepting_states)
   )
;


% for each day there must be at least 3 nurses with day shift,
% and 2 nurses with night shift
constraint
   forall(j in 1..num_days) (
      % add the availabile matrix
      sum(i in 1..num_nurses) ( bool2int(x[i,j] == day_shift /\ available[i,j] = 1) ) >= 3
      /\
      sum(i in 1..num_nurses) ( bool2int(x[i,j] == night_shift /\ available[i,j] = 1) ) >= 2
   )
;

% stats for each day
% and we can put the shift constrains here instead
constraint
  forall(j in 1..num_days) (
      forall(t in 1..2) (
          stat[j,t] = sum(i in 1..num_nurses) ( bool2int(x[i,j] == t /\ available[i,j] = 1) )
      )
      /\
      stat[j,off_shift] = sum(i in 1..num_nurses) ( bool2int(x[i,j] == off_shift) )
      /\
      stat[j,day_shift] >= 3 /\
      stat[j,night_shift] >= 2 

  )
;

% Enforce that not available must be off_shift.
constraint
  forall(i in 1..num_nurses, j in 1..num_days) (
     if available[i,j] = 0 then
        x[i,j] = off_shift
     else
        true
     endif
  )
;



output 
[
  "\nRostering:"
] ++
[
  if j = 1 then "\n" else " " endif ++
    if available[i,j] = 0 then
       show("-")
    else 
       show(days[fix(x[i,j])]) 
    endif
  | i in 1..num_nurses, j in 1..num_days
] ++ 
[
  "\n\nstatistics:\n" ++
  " d  n  o\n--------"
] ++
[
  if t = 1 then "\n" else "" endif ++
  show_int(2, stat[j,t]) ++ " "
  | j in 1..num_days, t in 1..3
] ++
["\n"];
