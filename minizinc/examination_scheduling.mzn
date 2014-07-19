% 
% Examination scheduling problem in MiniZinc.
% 
% Description from 
% Noppon Choosri: "Using grid puzzle to solve constraint-based scheduling problem"
% http://airccj.org/CSCP/vol4/csit42402.pdf
% """
% Problem definition: The problem is an examination scheduling problem. It is mainly concerned
% with assignment of subjects for exam into given time slot during examination period. The
% generated result shall be able to indicate the day of the week the exam is allocated together with
% the room assigned. The assumption of this problem is that this schedule is for a package
% registration system in which student in the same year will study the same subjects. The problem is
% concerned with practical constraints such as certain subjects requiring larger room and every
% student cannot take exams in more than 2 subjects in a day. Solving this problem manually, i.e.
% using human decision making, is highly time-consuming and prone to mistakes such as schedule
% conflicted issues. This research will apply the grid puzzle, shown in Figure 3, to tackle the
% described problem.

%  Business's rules                              Game's rules
%  -----------------------------------------------------------------------------------------------
%  A. All subjects have to be assigned to the    A. All the numbers indicating subject IDs, can be
%     schedule and each subject takes only 1     used only once
%     exam 
%  B. Students should not take more than 2 exams B. In a day sub-region, the number of assigned
%     in a same day                              subjects for each year cannot be over 2
%  C. Some subjects require large rooms          C The subjects that requires large rooms should be
%                                                assigned to the given area only
%
% ...
%
%  Table 2. Requirements of the problem
%  ID Year Large section (yes or no)
%  1  1    Yes
%  2  1    No
%  3  1    No
%  4  1    No
%  5  1    No
%  6  2    No
%  7  2    Yes
%  8  2    No
%  9  2    No
% 10  2    No
% 11  3    No
% 12  3    Yes
% 13  3    No
% 14  3    No
% 15  3    No
% 16  4    No
% 17  4    No
% 18  4    No
% 19  4    Yes
% 20  4    No
%
% ...
%
% """
%
% The schedule to fill (3 days, 3 slots per day, 6 rooms)
% Room r1 and r2 are large rooms
%
%        r1   r2   r3   r4   r5  r6
%        ----------------------------
% Day 1  |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%         --------------------------
% Day 2  |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%         --------------------------
% Day 3  |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%        |L    L    -    -    -   - |
%         --------------------------
%
% The solution from the paper:
%
%        r1   r2   r3   r4   r5  r6
%        ----------------------------
% Day 1  |19  12   20   15   10   9 |
%        | 3   1    -    -    -   - |
%        | -   -    -    -    -   - |
%         --------------------------
% Day 2  | 7   -   18   17   14  13 |
%        | -   -    8    5    4   - |
%        | -   -    -    -    -   - |
%         --------------------------
% Day 3  | -   -   16   11    6   2 |
%        | -   -    -    -    -   - |
%        | -   -    -    -    -   - |
%         --------------------------


%
% Here's one solution from this MiniZinc model, with the added objective to
% minimize the number of used rooms (z).
%
%   s:
%   10  9 11  0  0  0
%   12  5  4  0  0  0
%   16 17  0  0  0  0
%    8  7 13  0  0  0
%   14  3  2  0  0  0
%   18 19  0  0  0  0
%    6 15  0  0  0  0
%    1  0  0  0  0  0
%    0  0 20  0  0  0
%   slots: [8, 5, 5, 2, 2, 7, 4, 4, 1, 1, 1, 2, 4, 5, 7, 3, 3, 6, 6, 9]
%   days : [3, 2, 2, 1, 1, 3, 2, 2, 1, 1, 1, 1, 2, 2, 3, 1, 1, 2, 2, 3]
%   rooms: [1, 3, 2, 3, 2, 1, 2, 1, 2, 1, 3, 1, 3, 1, 2, 1, 2, 1, 2, 3]
%   z    : 3
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_days;
int: num_slots = 3*num_days;
int: num_rooms;
int: n;
int: num_years = max([data[i,2] | i in 1..n]);
set of int: large_rooms;

array[1..n, 1..3] of int: data;

set of int: large_rooms_ids = { i | i in 1..n where data[i,3] = 1 };

array[1..n] of int: years = [data[i,2] | i in 1..n];


% decision variables
array[1..num_slots, 1..num_rooms] of var 0..n: s;
array[1..n] of var 1..num_rooms: xrooms;
array[1..n] of var 1..num_slots: xslots;
array[1..n] of var 0..num_days: xdays;

% number of used rooms (perhaps to minimize)
var n div num_rooms..num_rooms: z = sum([ bool2int(sum([bool2int(xrooms[i] = room) | i in 1..n]) > 0 ) | room in 1..num_rooms]);

% solve satisfy;
solve :: int_search(
          [s[i,j] | i in 1..num_slots, j in 1..num_rooms]
          % ++ xrooms ++ xslots ++ xdays
          ,
          anti_first_fail,
          indomain_max, % indomain_median, 
          complete) 
         % satisfy;
         minimize z;

constraint

  % All id's are assigned to a room
  alldifferent_except_0([s[i,j] | i in 1..num_slots, j in 1..num_rooms]) 
  /\
  sum([bool2int(s[i,j]> 0) | i in 1..num_slots, j in 1..num_rooms] ) = n
  /\ 

  % large rooms assignments
  forall(i in 1..n) (
    if i in large_rooms_ids then
      xrooms[i] in large_rooms
    else
      true
    endif

    % connecting the s matrix with the x* arrays
    /\ s[xslots[i],xrooms[i]] = i 
    /\ xdays[i] = 1+( (xslots[i]-1) div num_days)
  )

  % students (id with the same year) should not take more than 2 exams
  % in the same day
  /\
  forall(y in 1..num_years, d in 1..num_days) (
     %% these two give the same failures for most solvers
     let {
        var 0..2: c
     } in
     count([xdays[i] | i in 1..n where years[i] = y], d, c)
     % sum([bool2int(xdays[i] = d ) | i in 1..n where years[i] = y]) <= 2
  )

  % symmetry breaking
  /\ value_precede_chain([i | i in 1..num_rooms], xrooms)
  /\ value_precede_chain([i | i in 1..num_days], xdays)

  %% implicit constraint: not faster
  % /\ alldifferent_except_0([(xslots[i]-1)*num_rooms + xrooms[i] | i in 1..n]) /\
 
;

output 
["s:"]
++
[
 if j = 1 then "\n" else " " endif ++
   show_int(2, s[i,j])

 | i in 1..num_slots, j in 1..num_rooms
]
++
[
 "\nslots: " ++ show(xslots) ++ "\n" ++
 "days : " ++ show(xdays) ++ "\n" ++
 "rooms: " ++ show(xrooms) ++ "\n" ++
 "z    : " ++ show(z) ++ "\n" ++
 "years (max 2 occurrences per day):"
]
++
[

 if j = 1 then "\nday " ++ show(1+ (i-1) div num_days) ++ ": " else " " endif ++
   if fix(s[i,j]) > 0 then
      show_int(2, years[fix(s[i,j])])
   else 
      " -"
   endif

 | i in 1..num_slots, j in 1..num_rooms


]
;

%
% data
%
n = 20;
num_days = 3;
num_rooms = 6;
large_rooms = {1,2};
% id, year, need large room (1=yes)
data = array2d(1..n, 1..3,
[
 1,1,1,
 2,1,0,
 3,1,0,
 4,1,0,
 5,1,0,

 6,2,0,
 7,2,1,
 8,2,0,
 9,2,0,
10,2,0,

11,3,0,
12,3,1,
13,3,0,
14,3,0,
15,3,0,

16,4,0,
17,4,0,
18,4,0,
19,4,1,
20,4,0
]);