% 
% "Becoming President with 22% of the Votes" in MiniZinc.
% 
% From David Curran (Live at the Witch Trials)
% "Becoming President with 22% of the Votes "
% http://liveatthewitchtrials.blogspot.ie/2012/11/becoming-president-with-22-of-votes.html
% """
% Political pundits talk about the possibility of winning the Presidential election with 
% less votes than your opponent. Assuming only two candidates what is the lowest 
% percentage of votes you could get and still win the election? In the case where 
% everyone votes this becomes an interesting question. In America some states have a 
% higher ratio of people to electoral college votes than others. If the ones that are 
% preferentially treated banded together a small percentage of people could decide the election.
% 
% In my last post 
% [http://liveatthewitchtrials.blogspot.ie/2012/11/a-search-for-smallest-winning-electoral.html]
% I created a program to work out in an overly complicated way what was the least amount of 
% land a president could be elected from in the US. In this post I want to figure out the 
% best states to win to get you 270+ electoral college seats using the smallest number of voters.
% 
% I got the estimated population in July 2011 from the census website here. Using this 
% data in the glpk program below and got the result shown in this map. If everyone voted 
% and the people who get the most power in votes all voted one way then the states on the 
% winning side would have 135936335 voters to get 270 electoral college seats. 
% The total population is 311591917 so 43.67% of the population 
%
% ...
% 
% The winner would win 39 districts of the 51 states+dc and lose Virginia, Georgia, 
% New Jersey, North Carolina [see comments below], Michigan, Ohio, Illinois, Pennsylvania, 
% Florida, New York, Texas, California.
% 
% Now if the candidate in these states just squeaked a win by one vote and got zero votes 
% in all the other states that means in a two party election you could win an election, 
% where everyone voted, with under 22% of the vote.
% 
% There are all sorts of other questions similar to this. What is the lowest number of states 
% you can win the Presidency with. What is the smallest block where all the states 
% touch? What is the shortest distance between all her state capitals a winner could 
% have? I think some analysis that included Senate and Congress seats could be 
% interesting. If you have any ideas please comment. 
% """

% This is a fairly straightforward translation of David's GLPK model and give the same

% These are the losing states according to David:
%   California
%   Florida
%   Georgia 
%   Illinois
%   Michigan
%   New Jersey
%   New York 
%   North Carolina <-- ?
%   Ohio
%   Pennsylvania
%   Texas
%   Virginia

% However, I can't find any solution where North Carolina is in the losing states
% (and z is still 135936335). The losing states according to this MiniZinc model is the 
% same as David's but without North Carolina
%   California
%   Florida
%   Georgia
%   Illinois
%   Michigan
%   New Jersey
%   New York
%   Ohio
%   Pennsylvania
%   Texas
%   Virginia
% 
% When minimizing z with North Carolina as a _losing state_ we get a value
% of 136088784 (higher than for the original model) and these losing states:
% 
%   California
%   Florida
%   Illinois
%   Missouri
%   New Jersey
%   New York
%   North Carolina
%   Ohio
%   Oregon
%   Pennsylvania
%   Texas
%   Virginia
% 
% Note: Georgina and Michigan are then no longer losing states, and we added
%       Missouri and Oregon.
%
% (I have tried to find any errors in this model compared to David's but 
%  haven't found any.)
% 
% 
% Later that very day (20121104): 
%    Problem solved: David initially misread the output of his model and
%    North Carolina should not be in the losing states. He has updated his blog post 
%    with this info (and a reference to this model).
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% code to find the least land area to get 270 votes. 
int: num_states = 51;

set of 1..num_states: STATES = 1..num_states;

%  parameters 
array[STATES] of int: Votes;
array[STATES] of int: Pop;
array[STATES] of string: StatesStr;
int: Need; 

% For checking alternative solutions (using MIP solvers)
int: num_sols = 1;
array[1..num_sols, STATES] of 0..1: sols = 
array2d(1..num_sols, STATES, 
[
% Winning:
% Alabama Alaska Arizona Arkansas Colorado Connecticut Delaware 'District of Columbia' Hawaii Idaho Indiana Iowa Kansas Kentucky Louisiana Maine Maryland Massachusetts Minnesota Mississippi Missouri Montana Nebraska Nevada 'New Hampshire' 'New Mexico' 'North Carolina' 'North Dakota' Oklahoma Oregon 'Rhode Island' 'South Carolina' 'South Dakota' Tennessee Utah Vermont Washington 'West Virginia' Wisconsin Wyoming 
% Losing:
% California Florida Georgia Illinois Michigan 'New Jersey' 'New York' Ohio Pennsylvania Texas Virginia 
1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1,


]);

%  decision variables: x1: alabama, x2: , x3: , x4:  x51: Wyoming
array[STATES] of var 0..1: x;
array[STATES] of var 0..max(Votes): y = [Votes[i]*x[i] | i in STATES];
var 0..sum(Pop): z = sum(i in STATES) (Pop[i]*x[i]);
var 0..num_states: s = sum(x); % number of selected states

% ann:var_select;
% ann:val_select;

% objective function
% solve  :: int_search(y, anti_first_fail, indomain_min, complete) minimize z;
solve :: int_search(y ++ x, largest, indomain_min, complete) minimize z;
% solve :: int_search(y ++ x, largest, indomain_min, complete) satisfy;
% solve :: int_search(y ++ x, var_select, val_select, complete) satisfy;

% solve minimize z;
% solve satisfy;


% Constraints
constraint
  % sum(i in STATES) (x[i]*Votes[i]) >= Need
  sum(i in STATES) (y[i]) >= Need % :: domain
  % /\ z = 135936335 % for checking unicity of the solution
  % /\ x[34] = 0 % North Carolina is a losing state

  % /\ % trying to find alternative solutions with z = 135936335
  % forall(i in 1..num_sols) (
  %   sum(j in STATES) ( abs(x[j] - sols[i,j])) > 0 % MIP
  %   % exists(j in STATES) ( x[j] != sols[i,j]) % CP
  % )
;

output 
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++ 
  "s: " ++ show(s) ++ "\n" ++
  "\nWinning:\n"
] ++
[
  if fix(x[i]) = 1 then
    show(StatesStr[i]) ++ " "
  else 
    ""
  endif
  | i in STATES
]
++
[ "\nLosing:\n" ] ++
[
  if fix(x[i]) = 0 then
    show(StatesStr[i]) ++ " "
  else 
    ""
  endif
  | i in STATES
]
;


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
14, % "New Jersey"
 5, % "New Mexico"
29, % "New York"
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


Pop = 
[
4802740, % Alabama
722718, % Alaska
6482505, % Arizona
2937979, % Arkansas
37691912, % California
5116796, % Colorado
3580709, % Connecticut
907135, % Delaware
617996, % "District of Columbia" 
19057542, % Florida
9815210, % Georgia
1374810, % Hawaii
1584985, % Idaho
12869257, % Illinois
6516922, % Indiana
3062309, % Iowa
2871238, % Kansas
4369356, % Kentucky
4574836, % Louisiana
1328188, % Maine
5828289, % Maryland
6587536, % Massachusetts
9876187, % Michigan
5344861, % Minnesota
2978512, % Mississippi
6010688, % Missouri
998199, % Montana
1842641, % Nebraska
2723322, % Nevada
1318194, % "New Hampshire" 
8821155, % "New Jersey" 
2082224, % "New Mexico" 
19465197, % "New York" 
9656401, % "North Carolina" 
683932, % "North Dakota" 
11544951, % Ohio
3791508, % Oklahoma
3871859, % Oregon
12742886, % Pennsylvania
1051302, % "Rhode Island"
4679230, % "South Carolina" 
824082, % "South Dakota"
6403353, % Tennessee
25674681, % Texas
2817222, % Utah
626431, % Vermont
8096604, % Virginia
6830038, % Washington
1855364, % "West Virginia" 
5711767, % Wisconsin
568158, % Wyoming
];

