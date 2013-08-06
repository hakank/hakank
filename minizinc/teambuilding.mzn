% 
% Teambuilding problem in MiniZinc.
% 
% From OPL model teambuilding.mod
% """
% Problem Description:  
% -------------------
% The HR department of a company organizes an integration day to welcome 
% new employees. 
% The problem is to configure 10 teams of 6 people that respect the 
% following rules:
% 
% There are 30 new employees and 30 existing employees. They work in 6 different 
% services lettered A to F.
% A team must have 3 existing employees and 3 new employees, and at most 4 
% people from the same service.
% Some new employees are coached by an existing employee, and an existing employee 
% can coach only one new employee.
%
% A new employee who is coached must be in the team of his coach.
% Furthermore, employees of services A and B cannot be in the same team; employees of 
% services E and F cannot be in the same team.
%
% Each person is represented by a number in 0-59; new employees are the even 
% numbers, existing employees are the odd numbers.
% 
% Service       Range
%   A          0-19 
%   B          20-39 
%   C          40-44
%   D          45-49
%   E          50-54
%   F          55-59

% In Service A: the couples coach/coached new employee are 
%               0-1, 2-3, 4-5, 6-7, 8-9, 10-11 
% In Service B: the couples coach/coached new employee are 
%               20-21, 22-23, 24-25, 26-27, 28-29, 30-31 
% In Services C,D,E,F, the couples coach/coached new employee are 
%               40-41, 42-43, 45-46, 47-48, 50-51, 52-53, 55-56, 57-58
%
%
% Additional constraints:
%
% Person number 5 must be with either person 41 or person 51.
% Person number 15 must be with either 40 or person 51.
% Person number 25 must be with either 40 or person 50.
% Furthermore, person 20 is with person 24, or person 22 is with person 50.
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_persons = 60;
set of int: persons = 0..num_persons-1;
int: num_teams = 10;
set of int: teams = 1..num_teams;

% The services
int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;

int: num_services = 6;
array[1..num_services] of set of int: service = 
   [
      0..19, % Service A
     20..39, %         B 
     40..44, %         C
     45..49, %         D
     50..54, %         E
     55..59  %         F
   ];

int: num_coaching = 20;
%  (coach, coached)
array[1..num_coaching, 1..2] of int: coaching = 
  array2d(1..num_coaching, 1..2, [
         0, 1,
         2, 3,
         4, 5,
         6, 7,
         8, 9,
        10,11,
        20,21,
        22,23,
        24,25,
        26,27,
        28,29,
        30,31,
        40,41,
        42,43,
        45,46,
        47,48, 
        50,51,
        52,53,
        55,56,
        57,58
    ]);



% decision variables
array[persons] of var teams: team;
% array[teams] of var set of persons: team2;
array[teams, persons] of var 0..1: team3;

% solve satisfy;
solve :: int_search(
                      team, 
                      first_fail, 
                      indomain_median, 
                      complete) satisfy;


constraint

  % global_cardinality(team, [i | i in 1..num_teams], [num_persons div num_teams | i in 1..num_teams]) /\

  % A team must have 3 existing employees and 3 new employees, and at most 
  % 4 people from the same service.
  forall(t in teams) (

     sum(existingemployee in persons where existingemployee mod 2 = 1) (
            bool2int(team[existingemployee] = t )
     ) = 3 

     /\

     sum(newemployee in persons where newemployee mod 2==0) ( 
            bool2int(team[newemployee]=t)
     ) = 3 

     /\
     forall(f in 1..num_services) (
       sum(person in service[f]) ( bool2int(team[person] == t) ) <=4 
     )
  )

  /\
  % Furthermore, employees of services A and B cannot be in the same team; 
  % employees of services E and F cannot be in the same team.
  forall(pA in service[A], pB in service[B]) (team[pA] != team[pB]) /\
  forall(pE in service[E], pF in service[F]) (team[pE] != team[pF]) /\
  
  % A new employee who is coached must be in the team of his coach.
  forall(c in 1..num_coaching) (team[coaching[c,1]]==team[coaching[c,2]]) /\
  
  % Person number 5 wants to be with either person 41 or person 51.
  (team[5] = team[41] \/ team[5] = team[51]) /\

  % Person number 15 wants to be with either 40 or person 51.
  (team[15] = team[40] \/ team[15] = team[51]) /\
  
  % Person number 25 wants to be with either 40 or person 50.
  (team[25] = team[40] \/ team[25] = team[50]) /\
  
  % Furthermore, person 20 is with person 24, or person 22 is with person 50.
  (team[20] = team[24] \/ team[22] = team[50])
;

% boolean matrix representation (for symmetry breaking)
constraint
  team[0] = 1 /\

  forall(t in teams, p in persons) (
      (team[p] = t <-> team3[t, p] = 1)
  )

  /\ % symmetry breaking: ordering of the first person in each team
  let {
    array[1..num_teams] of var persons: mins
  } in
  forall(t in 1..num_teams) (
    exists(p in persons) (
       mins[t] = p /\ 
       team3[t,p]=1 /\
       forall(i in persons where i < p) (
          team3[t, i] = 0
       )
       /\ (p > 1 -> mins[t] > mins[t-1]) 
     )
  )
  /\
  increasing(mins)

;



% set representation
% constraint
  % forall(t in teams) (
  %   card(team2[t]) = num_persons div num_teams
  % )
  % /\
  % forall(t in teams, p in persons) (
  %    (team[p] = t <-> p in team2[t])
  % )

  % /\ %% This don't work (as expected)
  % forall(t in 1..num_teams-1) (
  %    team2[t-1] < team2[t]
  % )
% ;



output [
 "Team " ++ show_int(2, t) ++ ": " ++
    show([p | p in persons where fix(team[p]) = t]) ++ "\n"
  | t in 1..num_teams
]
;

