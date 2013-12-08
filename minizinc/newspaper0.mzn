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

% This is a simplified version of 
%   http://www.hakank.org/minizinc/newspaper.mzn
% since it was too overloaded with presentation stuff.
  

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
% Cf the more general model
%   - http://www.hakank.org/minizinc/jobshop.mzn
%   - http://www.hakank.org/minizinc/jobshop2.mzn (more output)
%
% Data file that includes this newpaper problem:
%   - http://www.hakank.org/minizinc/jobshop_newspaper.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;


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
                  1)
    )
    % /\
    % forall(p in 1..n) (
    %    cumulative([reading_start[p,m] | m in 1..n],
    %               [reading_times[p,m] | m in 1..n],
    %               one,
    %               1)
    % )

   /\ % check the reading order
   forall(p in 1..n) (
      forall(m1,m2 in 1..n where m1 < m2) (
          if reading_order[p,m1] < reading_order[p,m2] then
             before(reading_end[p,m1], reading_start[p,m2])
          else 
             after(reading_start[p,m1], reading_end[p,m2])
          endif
      )
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
++
["\n"];