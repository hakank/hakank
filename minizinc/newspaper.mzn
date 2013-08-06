% 
% Newspaper problem in MiniZinc.
% 
% From
% http://cs.calstatela.edu/wiki/index.php/Courses/CS_460/Fall_2006/Patel,_Snehal/Newspaper
% (Includes the JaCoP model, distributed as ExamplesJaCoP.Newspaper.java)
% """
% There are four students: Algy, Bertie, Charlie and Digby, who share a flat. 
% Four newspapers are delivered to the house: the Financial Times, 
% the Guardian, the Daily Express and the Sun. Each of the students reads 
% all of the newspapers, in particular order and for a specified amount of 
% time (see below). Question: Given that Algy gets up at 8:30, 
% Bertie and Charlie at 8:45 and Digby at 9:30, what is the earliest 
% that they can all set off for college? 
%
%      Algy           Bertie        Charlie      Digby
% 1st  FT       60    Guardian 75   Express  5   Sun      90
% 2nd  Guardian 30    Express   3   Guardian 15  FT        1
% 3rd  Express   2    FT       25   FT       10  Guardian  1
% 4th  Sun       3    Sun      10   Sun      30  Express   1
% """
% 

% Extra requirements: All reads the newspaper in a specific order:
%  (Note: set enforce_reading_order to true)
% - Algy order   : - FT, Guardian, Express, Sun
% - Bertie order : - Guardian, Express, FT, Sun
% - Charlie order: - Express, Guardian, FT, Sun
% - Digby order  : - Sun, FT, Guardian, Express
%

% Notes:
%  *  This origin of this problem is from 
%     S. French: "Sequencing and Scheduling : 
%              an introduction to the mathematics of the
%              job-shop", Ellis Horwood Limited, 1982.
%
%  * Tim Duncan wrote about it in his paper
%    "Scheduling Problems and Constraint Logic Programming:
%     A Simple Example and its Solution", AIAI-TR-120, 1990,
%     page 5. 
%     (The paper also includes a program in CHIP 
%     solving the problem.)
%
%
  

% Optimal solution with extra requirement (one of many): 
%
%   Earliest end time: 180
%   (8:30 + 3 hours 0 minutes)
%   (i.e. at 11 and 30)
%
%   Times person reads newspapers:
%   Algy           : 110..139   45..104  140..141  175..179 
%   Bertie         :  35..109  113..137  110..112  165..174 
%   Charlie        :  20.. 34   35.. 44   15.. 19   45.. 74 
%   Digby          : 166..166  165..165  167..167   75..164 
%
%   Times newspapers is read:
%   Guardian       : 110..139   35..109   20.. 34  166..166 
%   Financial Time :  45..104  113..137   35.. 44  165..165 
%   Express        : 140..141  110..112   15.. 19  167..167 
%   Sun            : 175..179  165..174   45.. 74   75..164 
%


%
% For comparison, here is the solution of JaCoP's model 
% ExamplesJaCoP.Newspaper, see above (the output is edited)
%
%  start times:
%  algy    110  45 140 165
%  bertie   35 113 110 170
%  charlie  20  35  15  45
%  digby   166 165 167  75
%
%  makespan = 180
%



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;
bool: enforce_reading_order = true;

% Number of newspapers available (of each type)
% This is the limit parameter in cumulative
int: num_newspapers_available = 1; 

%
% The times they read the magazines
% 
array[1..n, 1..n] of int: reading_times = 
  array2d(1..n, 1..n, 
   [
    % Guard.  FT  Express  Sun
      30,    60,    2,      5,  % Algy
      75,    25,    3,     10,  % Bertie
      15,    10,    5,     30,  % Charlie
       1,     1,    1,     90   % Digby		
 ]);


%
% The order they read the newspapers:
%
% (1: Guardian, 2: Financial Time, 3: Express, 4: Sun)
%
% - Algy order   : - FT, Guardian, Express, Sun
% - Bertie order : - Guardian, Express, FT, Sun
% - Charlie order: - Express, Guardian, FT, Sun
% - Digby order  : - Sun, FT, Guardian, Express
%
array[1..n, 1..n] of int: reading_order = 
  array2d(1..n, 1..n,
  [% indicating the order in which each newspaper
   % must be read
   % Guardian  FT   Express  Sun
       2,      1,     3,     4,  % Algy
       1,      3,     2,     4,  % Bertie
       2,      3,     1,     4,  % Charlie
       3,      2,     4,     1   % Digby

  ]);

% for cumulative
array[1..n] of int: one = [1,1,1,1];
array[1..n] of int: two = [2,2,2,2];

% start times for each person
% Time 0 is 8:30
array[1..n] of int: start_times = [0,15,15,60];

array[1..n] of string: person     = ["Algy   ", 
                                     "Bertie ", 
                                     "Charlie", 
                                     "Digby  "];


array[1..n] of string: newspapers = ["Guardian      ", 
                                     "Financial Time", 
                                     "Express       ", 
                                     "Sun           "];


%
% readers x newspapers
% Start/End times for each person and each newspapers
%   Guardian, FinancialTime, Express, Sun
%
int: max_time = 200;
array[1..n, 1..n] of var 0..max_time: reading_start;
array[1..n, 1..n] of var 0..max_time: reading_end;

%
% The timeline, for simplifying the presentation: 
% Does person p reads something at time t?
%
array[0..max_time, 1..n] of var 0..1: timeline;

% The idle times (until all are finished reading)
% (experimental)
array[1..n] of var 0..max_time: idle_time;

var 0..max_time: earliest_end_time = max([reading_end[i,t] | i in 1..n, t in 1..n]);


% 
% t1 is before t2
% 
predicate before(var int: t1, var int: t2) =
    t1 <= t2
;

%
% t1 is after t2
%
predicate after(var int: t1, var int: t2) =
    t1 >= t2
;


% solve satisfy;
solve :: int_search(
        [reading_start[p,m] | p,m in 1..n],
        first_fail,
        indomain_min,
        complete
        )
     minimize earliest_end_time;
     % minimize sum(idle_time); % experimental
     % satisfy;


% for solve satisfy
% constraint 
%    if enforce_reading_order then 
%       earliest_end_time = 180
%    else 
%       earliest_end_time = 153
%    endif
% ;


constraint
       % earliest start times, i.e. after the person gets up
    forall(p in 1..n) (
        forall(m in 1..n) ( reading_start[p,m] >= start_times[p])
    ) 

    /\ % end times
    forall(p, m in 1..n) (
        reading_end[p,m] = reading_start[p,m] + reading_times[p,m]
    )

    /\ % ensure non-overlaps of the newspaper readings
    forall(m in 1..n) (
       cumulative([reading_start[p,m] | p in 1..n],
                  [reading_times[p,m] | p in 1..n],
                  one,
                  num_newspapers_available)
    )
    % /\
    % forall(p in 1..n) (
    %    cumulative([reading_start[p,m] | m in 1..n],
    %               [reading_times[p,m] | m in 1..n],
    %               one,
    %               1)
    % )

   /\ % check the reading order
   if enforce_reading_order then
      forall(p in 1..n) (
          forall(m1,m2 in 1..n where m1 < m2) (
              if reading_order[p,m1] < reading_order[p,m2] then
                 % reading_end[p,m1] <= reading_start[p,m2]
                 before(reading_end[p,m1], reading_start[p,m2])
              else 
                 % reading_start[p,m1] >=  reading_end[p,m2]
                 after(reading_start[p,m1], reading_end[p,m2])
              endif
          )
      )
   else 
     true
   endif

   /\ % For simplifying the presentation:
      % is person p reading anything at time t?
   forall(t in 0..max_time, p in 1..n) ( 
         exists(m in 1..n) (           
            t >= reading_start[p,m] 
            /\
            t <  reading_end[p,m]
         )  <-> timeline[t,p] = 1
   )

   /\ % For idle times until earliest_end_time (experimental)
   forall(p in 1..n) (
      idle_time[p] = sum(time in 0..max_time) ( 
                          bool2int(time <= earliest_end_time /\ timeline[time,p] = 0) ) 
   )
;


%
% Output
%
output 
[
  "\nEarliest end time: " ++ show(earliest_end_time) ++ "\n" ++
  "(8:30 + " ++ show(earliest_end_time div 60) ++ " hours " ++  show(earliest_end_time mod 60) ++ " minutes)\n" ++
  "(i.e. at " ++ show((earliest_end_time+510) div 60) ++ " and " ++  show((510+earliest_end_time) mod 60) ++ ")\n" ++
  "Start times: " ++ show(start_times) ++ "\n" ++
  "\nReading times:"
]
++
[ 
  if m = 1 then "\n" else " " endif ++
    show(reading_times[p,m])
  | p, m in 1..n
]
++ ["\n\nTimes person reads newspapers:"] ++
[
  if m = 1 then "\n" ++ show(person[p]) ++ " : "  else " " endif ++
     if fix(reading_start[p,m]) < 10 then " " else "" endif ++
     if fix(reading_start[p,m]) < 100 then " " else "" endif ++
       show(reading_start[p,m]) ++ ".." ++ 
       if fix(reading_end[p,m]) < 10 then " " else "" endif ++
       if fix(reading_end[p,m]) < 100 then " " else "" endif ++
     show(reading_end[p,m]-1) ++ " "
  | p,m in 1..n
]
++ [ "\n\nTimes newspapers is read:"] ++ 
[
  if p = 1 then "\n" ++ newspapers[m] ++ " : " else " " endif ++
     if fix(reading_start[p,m]) < 10 then " " else "" endif ++
     if fix(reading_start[p,m]) < 100 then " " else "" endif ++ 
    show(reading_start[p,m]) ++ ".." ++ 
       if fix(reading_end[p,m]) < 10 then " " else "" endif ++
       if fix(reading_end[p,m]) < 100 then " " else "" endif ++
    show(reading_end[p,m]-1) ++ " "
  | m,p in 1..n
]
++ ["\n\nSchedule:"] ++
[
    if time < fix(earliest_end_time) then 
      let { 
            int: tt = 510+time, % 8:30 -> 8*60 + 30 = 510 minutes
            int: hour = tt div 60,
            int: minute = tt mod 60
       }
       in 
      if m = 1 /\ p = 1 then "\nTime: " ++ 
                              show(time) ++ ": " ++ 
                              if time < 10 then " " else "" endif ++
                              if time < 100 then " " else "" endif ++
                              if hour < 10 then "0" else "" endif ++
                              show(hour) ++ ":" ++ 
                              if minute < 10 then "0" else "" endif ++
                              show(minute) 
                              ++ " "
           else "" endif ++

          if time >= fix(reading_start[p,m]) /\ time < fix(reading_end[p,m]) then
              show(person[p]) ++ ": " ++ show(newspapers[m]) ++ "  "
          else 
              if m = 1 /\ fix(timeline[time,p]) = 0 then
                 if time < start_times[p] then
                    show(person[p]) ++ ": <sleeping>      " 
                 else 
                    show(person[p]) ++ ": -               " 
                 endif
              else 
                 "" 
              endif
          endif
    else 
         ""
    endif
   | time in 0..max_time, p,m in 1..n
]
++
[
  "Sum idle times: " ++ show(idle_time) ++ "\n"
]
% ++
% [  % Just show the timeline
%    if p = 1 then "\n" else " " endif ++
%       show(timeline[time,p])
%   | time in 0..max_time, p in 1..n
% ] 
++
["\n"];