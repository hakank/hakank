% 
% Conference problem in MiniZinc.
% 
% From 
% "Constraint Programming with Alice"
% http://www.ps.uni-saarland.de/alice/manual/cptutorial/node37.html
% """
% We want to construct the time table of a conference. The conference will consist 
% of 11 sessions of equal length. The time table is to be organized as a sequence of 
% slots, where a slot can take up to 3 parallel sessions. There are the following 
% constraints on the timing of the sessions:
%
%  * Session 4 must take place before Session 11.
%  * Session 5 must take place before Session 10.
%  * Session 6 must take place before Session 11.
%  * Session 1 must not be in parallel with Sessions 2, 3, 5, 7, 8, and 10.
%  * Session 2 must not be in parallel with Sessions 3, 4, 7, 8, 9, and 11.
%  * Session 3 must not be in parallel with Sessions 5, 6, and 8.
%  * Session 4 must not be in parallel with Sessions 6, 8, and 10.
%  * Session 6 must not be in parallel with Sessions 7 and 10.
%  * Session 7 must not be in parallel with Sessions 8 and 9.
%  * Session 8 must not be in parallel with Session 10.
%
% The time table should minimize the number of slots. 
% 
% ...
%
% This plan says that the conference requires 4 slots, where 
%   the sessions 1, 4 and 9 take place in slot 1, 
%   the sessions 2, 5 and 6 take place in slot 2, 
%   the sessions 3, 7 and 10 take place in slot 3, and 
%   the sessions 8 and 11 take place in slot 4.
% """
%
% Using input_order and indomain_min we get exactly this solution:
% 
%    num_time_slots = 4
%    sessions = [1, 2, 3, 1, 2, 2, 3, 4, 1, 3, 4]
%    slots    = [{1, 4, 9}, {2, 5, 6}, {3, 7, 10}, {8, 11}, {}, {}, {}, {}, {}, {}, {}]
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: num_sessions = 11;
int: max_time_slots = 11;

% precedences of sessions
int: num_precedences = 3;
array[1..num_precedences, 1..2] of int: precedences = array2d(1..num_precedences, 1..2, [
   4,11,
   5,10,
   6,11
]);

% sessions that should not be parallel
int: num_para = 8;
array[1..num_para] of set of int: parallel = [
 {2, 3, 5, 7, 8, 10}, % not parallel with session 1
 {3, 4, 7, 8, 9, 11}, % not parallel with session 2
 {5, 6, 8},           % not parallel with session 3
 {6, 8, 10},          % not parallel with session 4
 {},                  % not parallel with session 5 (dummy)
 {7,10},              % not parallel with session 6
 {8, 9},              % not parallel with session 7
 {10}                 % not parallel with session 8
];


%
% decision variables
%
% sessions: in what slot is this session
array[1..num_sessions] of var 1..max_time_slots: sessions;

% slots: which sessions is in which slot
% we use set representation (dual) for two reasons:
% 1) for dual representation, since it is easy to state that 
%    there should be max 3 sessions per slot
% 2) for presentation
array[1..max_time_slots] of var set of 1..num_sessions: slots; 


% number of used time slots (to be minimized)
var 1..max_time_slots: num_time_slots = max(sessions);

% solve minimize num_time_slots;
solve :: int_search(sessions, input_order, indomain_min, complete) minimize num_time_slots;


% Precedences:
constraint
  forall(p in 1..num_precedences) (
     sessions[precedences[p,1]] < sessions[precedences[p,2]]
  )
;

% parallel constraints
constraint
  forall(s in 1..num_para where card(parallel[s]) > 0) (
     forall(pp in parallel[s]) (
        sessions[s] != sessions[pp]
     )
  )
;

% channel between sessions and slots
constraint
  int_set_channel(sessions, slots)
;

% max 3 sessions per slot
constraint
  forall(s in 1..max_time_slots) (
    card(slots[s]) <= 3
  )
;

output [
"sessions: " ++ show(sessions) ++ "\n"++
"slots: " ++ show(slots) ++ "\n"++
"num_time_slots: " ++ show(num_time_slots) ++ "\n"
];
