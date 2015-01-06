% 
% Muddle Management in MiniZinc.
% 
% http://l4f.cecs.anu.edu.au/guide/and-yet-more-introduction
% """
% Six managers of Muddle and Company have to schedule a series of meetings on 
% various aspects of the company’s activities. Not all of them are to attend 
% every meeting, but each of them has to attend three or four meetings. 
% The managers' names are 
%   Amy, Bernard, Chester, Dianne, Emma and Frank. 
%
% The meetings concern 
%   marketing, networks, orders, production, quality control, revenue, sales and timetables. 
% 
% These may take place in any order, except that 
%   the meeting on sales must happen before the one on marketing, 
%   and both production and quality control have to be discussed before the timetabling meeting 
% can take place. The meetings they are required to attend are:
% 
% Amy:  	networks 	quality 	revenue 	sales
% Bernard: 	marketing 	orders 	revenue 	sales
% Chester: 	networks 	production 	revenue 	timetables
% Dianne: 	production 	quality 	timetables 	–
% Emma: 	marketing 	orders 	quality 	timetables
% Frank: 	marketing 	networks 	timetables 	–
% 
% They want to schedule the meetings to fit into the smallest possible number of timeslots, 
% so as to get to their golf game straight after lunch. They find it hard to decide 
% how many timeslots they need and which meetings to hold in parallel with which.
% 
% Can you help them out?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;

int: marketing = 1;
int: networks = 2;
int: orders = 3;
int: production = 4;
int: quality = 5; % quality control
int: revenue = 6;
int: sales = 7;
int: timetables = 8;

int: num_meetings = 8;
array[1..num_meetings] of string: meetings_s = 
[
"marketing",
"networks",
"orders",
"production",
"quality",
"revenue",
"sales",
"timetables"
];

array[1..n] of set of int: required = 
[
{networks,quality,revenue,sales},         % Amy
{marketing,orders,revenue,sales},         % Bernard 
{networks,production,revenue,timetables}, % Chester
{production,quality,timetables}, 	  % Dianne
{marketing,orders,quality,timetables},    % Emma
{marketing,networks,timetables},          % Frank
];

int: num_slots = num_meetings; % maximum needed slots

% decision variables

% the time slots for each meeting (time start at 0)
array[1..num_meetings] of var 0..num_slots: x; 
var 0..num_slots: z = max(x); % max timeslot, to minimize

% solve minimize z;
solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) minimize z;

constraint
  forall(p in 1..n) (
    % find different timeslots for a person's meetings
    % forall(m1, m2 in required[p] where m1 < m2) (
    %    x[m1] != x[m2]
    % )
    all_different([x[m] | m in required[p]])
  )
  % precedence constraints
  /\ x[sales]      < x[marketing] 
  /\ x[production] < x[timetables]
  /\ x[quality]    < x[timetables]

  /\ z = 5 % for solve satisfy
;

output [
  "x: ", show(x), "\n",
  "z: ", show(z), "\n",
]
++
[
  show(meetings_s[m]) ++ " at time " ++ show(x[m]) ++ "\n"
  | m in 1..num_meetings
]
++
[
  if t <= fix(z) then 
    "time " ++ show(t) ++ ": " ++ show(join(", ",[meetings_s[m] | m in 1..num_meetings where fix(x[m]) = t])) ++ "\n"
  else 
   ""
  endif

  | t in 1..num_meetings
]
;

