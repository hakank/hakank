% 
% Company competition in MiniZinc.
%
% This model show configurations of teams for our Christmas company 
% competition (in 2010 it's bowling) with the following rules of thumb:
%
%   - 18 persons, with 4 (or 5) persons in each team -> 4 teams
%   - even distribution of sexes in each team
%   - even distribution of departments in each team
%   - the managers should be in different teams, if possible
%
% The number of violations of these rules is minimzed via the
% variable z.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% it
int: hakan   = 1; % that's me:-)
int: andersj = 2;
int: robert  = 3;
int: markus  = 4;
int: johan   = 5;
int: micke   = 6;

% customer services 1
int: alex    = 7;
int: andersh = 8;
int: jennyk  = 9;
int: kenneth = 10;
int: sara    = 11;
int: cecilia = 12;

% customer services 2
int: stefan  = 13;
int: jacob   = 14;
int: roger   = 15;
int: henrik  = 16;
int: line    = 17;
int: hanna   = 18;


% total number of persons
int: n = 18;

int: it = 1;
int: customer_services1 = 2;
int: customer_services2 = 3;

int: num_departments = 3;
array[1..n] of 1..num_departments: departments = 
                   % 1 2 3 4 5 6   7 8 9 0 1 2    3 4 5 6 7 8
                   [ 1,1,1,1,1,1,  2,2,2,2,2,2,   3,3,3,3,3,3];

% array[1..num_departments] of var 0..n: size_departments;
array[1..num_departments] of int: size_departments = [sum([1 | i in 1..n where departments[i] == d]) | d in 1..num_departments];
array[1..num_departments] of string: dept_str = ["it", "cr1","cr2"];

int: M = 1;
int: F = 2;
array[1..n] of M..F: sex =
              % 1 2 3 4 5 6   7 8 9 0 1 2    3 4 5 6 7 8
              [ M,M,M,M,M,M,  F,M,F,M,F,F,   M,M,M,M,F,F];
% Male: 12
% Female: 6

array[1..2] of string: sex_str = ["M","F"];

% managers should be in different teams
set of int: managers = {johan, cecilia, stefan};

array[1..n] of string: who =
[
"hakan","andersj","robert","markus","johan","micke",  % it
"alex","andersh","jennyk","kenneth","sara","cecilia", % cr1
"stefan","jacob","roger","henrik","line","hanna"      % cr2 
];

% t_size 3, 5, and 6 is quite fast 
% howver t_size 4 is harder
int: t_size = 4; % about t_size persons in each team
int: num_teams = n div t_size;


%
% decision variables
%
% which persons belong to which team
array[1..num_teams] of var set of 1..n: team;
% number of persons from each departments in each team
array[1..num_teams, 1..num_departments] of var 1..n: team_departments;
% number of each sex in each team
array[1..num_teams, M..F] of var 1..n: team_sex;

% size of the team
% Note that we just accept sizes of t_size or t_size+1 
array[1..num_teams] of var t_size..t_size+1: team_size;

% which team to assign a person? <-> channelling team
array[1..n] of var 1..num_teams: which_team;

% number of "inequalities" of even distribution in sex and departments
% This may be minimized
var 0..100: z;

% solve satisfy;
% solve minimize z;

solve :: seq_search(
    [
     int_search(
          which_team ++
          [team_departments[t,d] | t in 1..num_teams, d in 1..num_departments] %++
          % [team_sex[t,s] | t in 1..num_teams, s in M..F]


          % ++ [team_departments[t,d] | t in 1..num_teams, d in 1..num_departments]
          , 
          first_fail, 
          indomain_min,
          complete)
     ,
     set_search(team, first_fail, indomain_min, complete),
     ]

   ) 
   minimize z;
   % satisfy;


% convenience predicate
%    mmin <= x <= mmax
predicate between(var int: x, int: mmin, int: mmax) =
   x >= mmin
   /\
   x <= mmax
;

constraint
  
  all_disjoint(team)
  /\
  forall(t in 1..num_teams) (
    % allow some differences in the size of team
    team_size[t] = card(team[t])
    /\
    between(team_size[t], t_size, t_size+1)

    /\  % channel between team and which_team
    forall(i in 1..n)  ( 
       i in team[t] <-> which_team[i] = t
    )
    /\
    forall(d in 1..num_departments) (
      team_departments[t, d] = sum(i in 1..n where departments[i] == d) (bool2int(i in team[t])) 
      % /\
      % team_departments[t, d] > 0
    )

    /\ % at least 1 person of each sex in each team
    forall(s in M..F) (
      team_sex[t, s] = sum(i in 1..n where sex[i] == s) (bool2int(i in team[t])) 
      % /\ 
      % team_sex[t, s] > 0
    )
  ) 

  /\ % count the number of inequalities between sexes and departments
     % and - if possible - separate the managers
  z = 
      % distribution of sexes
      % sum(t in 1..num_teams, s in 1..2) (abs(team_sex[t,s] - team_size[t] div 2)) 
      %+
      sum(t in 1..num_teams) (abs(team_sex[t,M] - team_sex[t,F])) 
 
      %+
      %sum(t1, t2 in 1..num_teams) (abs(team_sex[t1,M] - team_sex[t2,M])) 
      %+
      %sum(t1, t2 in 1..num_teams) (abs(team_sex[t1,F] - team_sex[t2,F])) 

      % + % distribution of departments     
      % sum(t in 1..num_teams, d in 1..num_departments) (abs(team_departments[t,d] - (team_size[t] div num_departments)))
      +
      sum(t in 1..num_teams, d1,d2 in 1..num_departments where d1 < d2) (abs(team_departments[t,d1] - team_departments[t,d2]))
      %+
      %sum(t1,t2 in 1..num_teams, d in 1..num_departments where t1 != t2) (abs(team_departments[t1,d] - team_departments[t2,d]))

      + % separation of managers
      sum([bool2int(which_team[m1] = which_team[m2]) | m1, m2 in managers where m1 < m2])


  /\ % some symmetry breaking
     % place the first IT guys in each team (as far as possible)
  forall(i in 1..num_teams) (
    i in team[i]
  )

  /\ % symmetry breaking: team_size should be decresaing 
  forall(i in 2..num_teams) (
    team_size[i-1] >= team_size[i]
  )

  /\ % Gecode/fz don't like this: Constraint set_le not found
  increasing_set(team)

  % for solve satisfy
  % /\ z = 14
  % /\ z = 12
  % /\ z = 6 % t_size = 5
;

output[
  "\nz (#violations): ", show(z), "\n",
  "\nTeams: ", show(team), "\n",
] ++ 
[ "\nTeam Departments:"] ++
[
  if d = 1 then "\n" else " " endif ++
    show(team_departments[t,d])
  | t in 1..num_teams, d in 1..num_departments
] ++ 
[ "\n\nTeam Sexes:"] ++
[
  if s = 1 then "\n" else " " endif ++
    show(team_sex[t,s])
  | t in 1..num_teams, s in M..F
] ++ 
[ "\n\nTeam_size: ", show(team_size), "\n"] ++
[ "\nwhich_team: ", show(which_team), "\n"] ++
[
  show(i) ++ ": " ++ show(who[i]) ++ "\t" ++ show(which_team[i]) ++ "\n"
  | i in 1..n
] ++
[ "\nThe teams:"] ++
[
  if p = 1 then "\nTeam " ++ show(t) ++ ": " else "" endif ++
  if fix(which_team[p]) == t then show(who[p]) ++ 
                                  "(" ++ sex_str[sex[p]] ++ "," ++ 
                                  dept_str[departments[p]] ++ ") " 
                             else "" endif
  | t in 1..num_teams, p in 1..n
] ++
[
  "\n\nManagers:\n"
] ++ 
[
  show(who[m]) ++ "(" ++ dept_str[departments[m]] ++ ") belongs to team " ++ show(which_team[m]) ++ "\n"
  | m in managers
] ++
["\n"];

