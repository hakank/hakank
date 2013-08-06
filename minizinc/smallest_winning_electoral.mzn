% 
% "Least amount of land that will make you President" in MiniZinc.
% 
% From David Curran (Live at the Witch Trials)
% "What is the least amount of land that will make you President? "
% http://liveatthewitchtrials.blogspot.ie/2012/11/a-search-for-smallest-winning-electoral.html
% """
% Matthew Yglesias in the slate
% [http://www.slate.com/blogs/moneybox/2012/10/31/geographically_smallest_electoral_college_map.html]
%  calculates what he thinks is the smallest area of land 
% a candidate could win and still win this presidential election. Densely populated 
% states will have more electoral college votes per square kilometer and so you can 
% win the election while winning a relatively small surface area of America. 
% 
% His reasoning is 'I started with a list of states in order of population density. 
% So you have DC, then New Jersey, then Rhode Island, then Massachusetts, and so forth. 
% Eventually you get a set that wins you the electoral college. Except the bloc of the 
% 18 densest states gives you 282 electoral votes—way more than you need. 
% Eliminate Michigan, the 18th densest, and you have 266 electoral votes. So then you can 
% round things out with little New Hampshire's four electoral votes and you have your winning map'.
% 
% I checked this allocation with the GLPK program below. I used the electoral votes listed 
% here and the state areas listed on wikipedia This gets 270 votes with an area of 
% 1625012km². US states + DC is an area of 9826630km² so 16.54% of the US could win an election.
% 
% The states are Delaware, District of Columbia, Hawaii, New Hampshire, Rhode Island, 
% Connecticut, Maryland, Indiana, Massachusetts, Virginia, New Jersey, North Carolina, 
% Ohio, Illinois, Pennsylvania, Florida, New York, California. My map is here
% 
% Matthew Yglesias' map is the same so he did find the optimal solution by hand. 
% """

% This is a fairly straightforward translation of David's GLPK model and give the same
% solution: 
%   California
%   Connecticut, 
%   Delaware, 
%   District of Columbia, 
%   Florida, 
%   Hawaii, 
%   Illinois, 
%   Indiana, 
%   Maryland, 
%   Massachusetts, 
%   New Hampshire, 
%   New Jersey, 
%   New York, 
%   North Carolina, 
%   Ohio, 
%   Pennsylvania, 
%   Rhode Island, 
%   Virginia, 
% 
% z (optimal cost) is 1625012 and it's a unique solution.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% code to find the least land area to get 270 votes. 
int: num_states = 51;

set of 1..num_states: STATES = 1..num_states;
set of 1..num_states: NEED = 1..num_states;

%  parameters 
array[STATES] of int: Votes;
array[STATES] of int: Cost;
array[STATES] of string: StatesStr;
int: Need; 

%  decision variables: x1: alabama, x2: , x3: , x4:  x51: Wyoming
array[STATES] of var 0..1: x;
array[STATES] of var 0..max(Cost): y = [Votes[i]*x[i] | i in STATES];
var 0..sum(Cost): z = sum(i in STATES) (Cost[i]*x[i]);
var 0..num_states: s = sum(x); % number of selected states

% objective function
solve  :: int_search(y, anti_first_fail, indomain_max, complete) minimize z;
% solve :: int_search(y, anti_first_fail, indomain_max, complete) satisfy;

% Constraints
constraint
  % z >= 0 /\
  sum(i in STATES) (y[i]) >= Need
  % /\ z = 1625012 % check unicity of the solution
;

constraint
  trace("sum(Cost): " ++ show(sum(Cost)) ++ "\n", 1=1)
;

output 
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++ 
  "s: " ++ show(s) ++ "\n"
] ++
[
  if fix(x[i]) = 1 then
    show(StatesStr[i]) ++ " "
  else 
    ""
  endif
  | i in STATES
];


%
% Data
%

Need = 270;

StatesStr = [
 "Alabama",
 "Alaska",
 "Arizona",
 "Arkansas",
 "California",
 "Colorado",
 "Connecticut",
 "Delaware",
 "'District of Columbia'",
 "Florida",
 "Georgia",
 "Hawaii",
 "Idaho",
 "Illinois",
 "Indiana",
 "Iowa",
 "Kansas",
 "Kentucky",
 "Louisiana",
 "Maine",
 "Maryland",
 "Massachusetts",
 "Michigan",
 "Minnesota",
 "Mississippi",
 "Missouri",
 "Montana",
 "Nebraska",
 "Nevada",
 "'New Hampshire'",
 "'New Jersey'",
 "'New Mexico'",
 "'New York'",
 "'North Carolina'",
 "'North Dakota'",
 "Ohio",
 "Oklahoma",
 "Oregon",
 "Pennsylvania",
 "'Rhode Island'",
 "'South Carolina'",
 "'South Dakota'",
 "Tennessee",
 "Texas",
 "Utah",
 "Vermont",
 "Virginia",
 "Washington",
 "'West Virginia'",
 "Wisconsin",
 "Wyoming",
];


Votes = [
 9, % Alabama
 3, % Alaska 
11, % Arizona
 6, % Arkansas
55, % California
 9, % Colorado 
 7, % Connecticut
 3, % Delaware
 3, % "District of Columbia"
29, % Florida
16, % Georgia
 4, % Hawaii
 4, % Idaho
20, % Illinois
11, % Indiana
 6, % Iowa
 6, % Kansas
 8, % Kentucky
 8, % Louisiana
 4, % Maine
10, % Maryland
11, % Massachusetts
16, % Michigan
10, % Minnesota
 6, % Mississippi
10, % Missouri
 3, % Montana
 5, % Nebraska
 6, % Nevada
 4, % "New Hampshire"
14, % "New, % Jersey"
 5, % "New Mexico"
29, % "New, % York"
15, % "North Carolina"
 3, % "North Dakota",
18, % Ohio 
 7, % Oklahoma
 7, % Oregon
20, % Pennsylvania
 4, % "Rhode Island" 
 9, % "South Carolina" 
 3, % "South Dakota" 
 11,%  Tennessee
38, % Texas
 6, % Utah
 3, % Vermont
13, % Virginia
12, % Washington
 5, % "West Virginia"
10, % Wisconsin
 3, % Wyoming
];

Cost = 
[
135765, % Alabama
1717854, % Alaska
295254, % Arizona
137732, % Arkansas
423970, % California
269601, % Colorado
14357, % Connecticut
6447, % Delaware
177, % "District of Columbia"
170304, % Florida
153909, % Georgia
28311, % Hawaii
216446, % Idaho
149998, % Illinois
94321, % Indiana
145743, % Iowa
213096, % Kansas
104659, % Kentucky
134264, % Louisiana
91646, % Maine
32133, % Maryland
27336, % Massachusetts
250494, % Michigan
225171, % Minnesota
125434, % Mississippi
180533, % Missouri
380838, % Montana
200345, % Nebraska
286351, % Nevada
24216, % "New Hampshire"
22588, % "New, Jersey"
314915, % "New Mexico"
141299, % "New York"
139389, % "North Carolina"
183112, % "North Dakota"
116096, % Ohio
181035, % Oklahoma
254805, % Oregon
119283, % Pennsylvania
4002, % "Rhode Island"
82932, % "South Carolina"
199731, % "South Dakota"
109151, % Tennessee
695621, % Texas
219887, % Utah
24901, % Vermont
110785, % Virginia
184665, % Washington
62755, % "West Virginia"
169639, % Wisconsin
253336, % Wyoming
];



