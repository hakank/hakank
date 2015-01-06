% 
% Fair Xmas duty (2014) in MiniZinc.
% 
% In 2014/15 a (fictive) Swedish company have a special Xmas duty from 
% Dec 24 to Jan 6. Here's a model for scheduling this as fair as possible.
% 
% The related holidays for Swedish Xmas and New Year's are:
%  * Dec 24 Christmas Eve (this is the day we distribute Xmas gifts)
%  * Dec 25 Christmas Day
%  * Dec 26 Boxing Day
%  * Dec 31 New Year's Eve
%  * Jan 1 New Year's Day
%  * Jan 6 Thirteen's Day (Epiphany) 
%
% The assumptions:
% * all days have a point of at least 1, but some days are more valuable than 
%   others and thus have higher points:
%     Workday and Xmas Eve: 2 points
%     New Year's Eve: 3 points
% * a person should not be on duty for more than two days in a row.
% * there are some "forbidden" days, i.e. a day where a person
%   for some reason cannot be on duty
% * optimization is on the total differences of the points
% * the number of days on duty is not relevant, it's the points
%   that counts.
%

%
% Here is one optimal solution:
%
%   total_diffs: 3
%   Dec 24 Wed Xmas Eve                   (2p): Boris
%   Dec 25 Thu Xmas Day                   (1p): Adam
%   Dec 26 Fri Boxing Day                 (1p): Adam
%   Dec 27 Sat                            (1p): Cecile
%   Dec 28 Sun                            (1p): Adam
%   Dec 29 Mon                            (2p): Adam
%   Dec 30 Tue                            (2p): Cecile
%   Dec 31 Wed New Year's Eve             (3p): Boris
%   Jan 1  Thu New Year's Day             (1p): Cecile
%   Jan 2  Fri                            (2p): Danielle
%   Jan 3  Sat                            (1p): Adam
%   Jan 4  Sun                            (1p): Danielle
%   Jan 5  Mon                            (2p): Danielle
%   Jan 6  Tue Thirteenth Day (Epiphany)  (1p): Cecile
% 
%   Points:
%   Adam: 6 points (5 days)
%   Boris: 5 points (2 days)
%   Cecile: 5 points (4 days)
%   Danielle: 5 points (3 days)
%
% There are 10253 optimal solutions (with total_diff = 3)
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_days = 14;

% 
% The points are the cost we estimate for being on duty these days.
%
%  1: Normal (holi)day
%  2: Workday and Xmas Eve
%  3: New Year's Eve
%
% The calendar is for 2014
%
array[1..num_days] of int: points =
[
 2, % Dec 24 Wed Xmas Eve
 1, % Dec 25 Thu Xmas Day
 1, % Dec 26 Fri Boxing Day
 1, % Dec 27 Sat
 1, % Dec 28 Sun
 2, % Dec 29 Mon
 2, % Dec 30 Tue
 3, % Dec 31 Wed New Year's Eve
 1, % Jan 1  Thu New Year's Day
 2, % Jan 2  Fri
 1, % Jan 3  Sat
 1, % Jan 4  Sun
 2, % Jan 5  Mon
 1, % Jan 6  Tue Thirteenth Day (Epiphany)
];

array[1..num_days] of string: days_str = 
[
 "Dec 24 Wed Xmas Eve                  ",
 "Dec 25 Thu Xmas Day                  ",
 "Dec 26 Fri Boxing Day                ",
 "Dec 27 Sat                           ",
 "Dec 28 Sun                           ",
 "Dec 29 Mon                           ",
 "Dec 30 Tue                           ",
 "Dec 31 Wed New Year's Eve            ",
 "Jan 1  Thu New Year's Day            ",
 "Jan 2  Fri                           ",
 "Jan 3  Sat                           ",
 "Jan 4  Sun                           ",
 "Jan 5  Mon                           ",
 "Jan 6  Tue Thirteenth Day (Epiphany) ",

];

int: adam     = 1;
int: boris    = 2;
int: cecile   = 3;
int: danielle = 4;

int: num_people = 4;
array[1..num_people] of string: people_str = ["Adam","Boris","Cecile","Danielle"];

%
% Forbidden assignments: the days a person cannot work (for some reason).
%
int: christmas_eve = 1;
int: christmas_day = 2;
int: boxing_day    = 3;
int: dec30         = 7;
int: new_years_eve = 8;
int: new_years_day = 9;
int: epiphany      = 14;
array[1..num_people] of set of int: forbidden =
[
  {christmas_eve,new_years_eve,new_years_day}, % adam
  {dec30,epiphany}, % boris
  {christmas_eve,christmas_day,new_years_eve}, % cecile
  {christmas_eve,christmas_day,new_years_eve,epiphany} % danielle
];

%
% decision variables
%

% who is on duty on day d
array[1..num_days] of var 1..num_people: on_duty;

% duty points for each person
array[1..num_people] of var 1..3*num_days: people_points;
% number of days on duty (not used in optimization)
array[1..num_people] of var 1..num_days: people_days;

%
% Total differences between the points of each pair of people
% (to minimize).
%

% Initialization with the sum don't work in MiniZinc 2.0. mzn2fzn complains about:
% """
% mzn2fzn: /home/hakank/g12/git/libminizinc/lib/eval_par.cpp:1503: void MiniZinc::ComputeIntBounds::vCall(MiniZinc::Call&): Assertion  stacktop+al->v().size()==_bounds.size()' failed.
% Aborted (core dumped)
% """
% It works if the sum is in a constraint section. (And it works MiniZinc 1.6.)
var int: total_diffs; %  = sum([abs(people_points[i]-people_points[j]) | i,j in 1..num_people where i < j ]);

% solve minimize total_diffs;
solve :: int_search(on_duty++people_points, smallest, indomain_min, complete) minimize total_diffs;
% solve :: int_search(on_duty++people_points, smallest, indomain_min, complete) satisfy;


constraint

  total_diffs = sum([abs(people_points[i]-people_points[j]) | i,j in 1..num_people where i < j ]) /\
  % total_diffs = 3 /\ % for solve satisfy

  % calculate the points for all people
  % Note: we don't punish if a person works two days in a row. Perhaps we should?
  forall(p in 1..num_people) (
    people_points[p] = sum([points[d]*bool2int(on_duty[d] = p) | d in 1..num_days]) /\
    people_days[p] = sum([bool2int(on_duty[d] = p) | d in 1..num_days])
  )
  /\ % handle forbidden assignments
  forall(p in 1..num_people) (
     forall(d in forbidden[p]) (
       on_duty[d] != p
     )
  )
  % /\ % a person should not be on duty for more that two days in row
  % not(exists(d in 1..num_days-2) (
  %    on_duty[d] = on_duty[d+1] /\
  %    on_duty[d] = on_duty[d+2]
  % ))
  /\ % this is slightly faster
  forall(p in 1..num_people) (
    forall(d in 1..num_days-2) (
      sum([bool2int(on_duty[d+i]=p) | i in 0..2]) <= 2
    )
  )
;

output [
  "total_diffs: ", show(total_diffs), "\n",
  % "on_duty: ", show(on_duty), "\n",
  % "people_points: ", show(people_points), "\n",
]
++
[
 show(days_str[d]) ++ " (" ++ show(points[d]) ++  "p): " ++ show(people_str[fix(on_duty[d])]) ++ "\n"
 | d in 1..num_days
]
++ ["\nPoints:\n"] ++
[
  show(people_str[p]) ++ ": " ++ show(people_points[p]) ++ " points " ++ "(" ++ show(people_days[p]) ++ " days)\n"
  | p in 1..num_people
]
;

