% 
% Carpool fairness in MiniZinc.
% 
% From
% "Applications of the maximum flow problem"
% http://people.orie.cornell.edu/dpw/orie633/LectureNotes/lecture2.pdf
% """
% 1.2
% Carpool Fairness
% Description: In this scenario, n people are sharing a carpool for m days. Each person
% may choose whether to participate in the carpool on each day.
% Example. The following table describes a carpool in which 4 people share a carpool 5
% days. X’s indicate days when people participate in the carpool.
%  
%  Person   Days:  1  2  3  4  5
%     1            X  X  X
%     2            X     X
%     3            X  X  X  X  X
%     4               X  X  X  X
%
% Our goal is to allocate the daily driving responsibilities 'fairly'. One possible approach
% is to split the responsibilities based on how many people use the car. So, on a day when k
% people use the carpool, each person incurs a responsibility of k . That is, for each person
% i, we calculate his or her driving obligation Oi as shown below. We can then require that
% person i drives no more than Oi times every m days. Table 1.2 shows the calculation of
% these Oi and their ceilings.
%   
% Table 1: Driver Responsibilities
%  Person   Days:  1    2    3    4    5    Oi    ceil(Oi)
%     1            1/3  1/3  1/4            1     1
%     2            1/3       1/4            7/12  1
%     3            1/3  1/3  1/4  1/2  1/2  7/4   2
%     4                 1/3  1/4  1/2  1/2  19/12 2
%     Sum          1    1    1    1    1    -     -
%
% """
%
% This model is a CP approach inspired the above, especially
% """
% We can then require that person i drives no more than Oi times every m days
% """
%
% Here we minimize the differences between Oi and the actual value to get
% as fair schedule as possible,
%
% A possible solution to the above problem:
%   z: 1
%   pdiff: 3
%
%   Days:
%   day  1 driver: 1
%   day  2 driver: 3
%   day  3 driver: 2
%   day  4 driver: 3
%   day  5 driver: 4
% 
%   People:
%   person1 (obl: 1 drives: 1)  days: 1
%   person2 (obl: 1 drives: 1)  days: 3
%   person3 (obl: 2 drives: 2)  days: 2,4
%   person4 (obl: 2 drives: 1!)  days: 5
% 
%   Schedule: 'X': drives, 'r': rides, '_': don't participates the day
%   X r r _ _
%   r _ X _ _
%   r X r X r
%   _ r r r X
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_days;
int: num_people;

% the days a person want to car pool (and perhaps drive)
array[1..num_people, 1..num_days] of int: participation;

array[1..num_days] of int: day_people = [sum([participation[p,d] | p in 1..num_people]) | d in 1..num_days ];

% the number of days a person should drive to be fair
array[1..num_people] of int: obligation = 
  [ceil(sum([1/day_people[d]*participation[p,d] | d in 1..num_days])) | p in 1..num_people];

int: obligation_diff = sum(obligation) - num_days;


% decision variables

% who will drive at day d
array[1..num_days] of var 1..num_people: driver;

% how many days drives person p
array[1..num_people] of var 1..num_days: num_drives;
array[1..num_people] of var 0..num_people div abs(num_days-sum(obligation)): diffs;

% the objective: number of discrepances between the
% actual driving and the number in obligation[p]
% var 0..num_people*num_days: z = sum(diffs);
var 0..obligation_diff: z = sum(diffs);

var 0..num_people*num_days: pdiff;

var int: mean = sum(diffs) div num_people;

% solve minimize z;
solve :: int_search(diffs ++ driver ++ num_drives, occurrence, indomain_median, complete) minimize pdiff;

constraint
  % person p don't drive no more than obligation[p] days per num_days
  forall(p in 1..num_people) (
    num_drives[p] = sum([bool2int(driver[d]=p)| d in 1..num_days])
    /\
    num_drives[p] <= obligation[p]
    /\
    diffs[p] = abs(obligation[p]-num_drives[p])
  )
  % /\ z = sum(obligation)-num_days
  /\
  z = obligation_diff
  /\
  pdiff = sum([abs(diffs[p1]-diffs[p2]) | p1,p2 in 1..num_people where p1 < p2])
  /\ % a person can only drive when (s)he participates
  forall(d in 1..num_days) (
    driver[d] in { p | p in 1..num_people where participation[p,d] = 1}
    % forall(p in 1..num_people where participation[p,d] = 0) (
    %   driver[d] != p
    % )
  )

  % symmetry breaking
  % /\ value_precede_chain([1,2,3,4,5,6,7,8], driver)

  % increasing(diffs)
;

output [
  "num_people: ", show(num_people), "\n",
  "num_people div abs(num_days-sum(obligation)): ", show(num_people div abs(num_days-sum(obligation))), "\n",
  "num_days: ", show(num_days), "\n",
  "oligation: ",show(obligation), "\n",
  "sum_oligation: ",show(sum(obligation)), "\n",
  "driver: ", show(driver), "\n",
  "num_drives: ", show(num_drives), "\n",
  "diffs: ", show(diffs), "\n",
  "mean: ", show(mean), "\n",
  "z: ", show(z), "\n",
  "pdiff: ", show(pdiff), "\n"
]
++ ["\nDays:\n"] ++
[
  "day " ++ show_int(2,d) ++ " driver: " ++ show(driver[d]) ++ "\n"
  | d in 1..num_days
]
++ ["\nPeople:\n"] ++
[
  "person" ++ show(p) ++ 
  " (obl: " ++ show(obligation[p]) ++ " drives: " ++ show(num_drives[p]) ++  if obligation[p] != fix(num_drives[p]) then "!" else "" endif ++ ") " ++
  " days: " ++ 
         show(join(",", [show(d) | d in 1..num_days where fix(driver[d]) = p])) ++ "\n"
  | p in 1..num_people
]
++ ["\nSchedule: 'X': drives, 'r': rides, '_': don't participates the day"] ++
[
  if d = 1 then "\n" else " " endif ++
    if fix(driver[d] = p) then "X" 
    elseif participation[p,d] = 1 then "r"
    else "_"
    endif
  | p in 1..num_people, d in 1..num_days
]
;


%
% data
%

% original problem: 1 week
% num_days = 5;
% num_people = 4;
% participation = array2d(1..num_people, 1..num_days,
%   [ 
%     % days 1 2 3 4 5
%            1,1,1,0,0, % p 1
%            1,0,1,0,0, % p 2
%            1,1,1,1,1, % p 3
%            0,1,1,1,1, % p 4
%   ]);
