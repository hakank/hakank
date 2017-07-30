% 
% Tourist site competition in MiniZinc.
%
% From Pierre Flener's presentation 
% "Constraint Technology - A Programming Paradigm on the Rise"
% http://www.it.uu.se/edu/course/homepage/ai/vt08/AI-CT.pdf
%    pages 5f: problem statement 
%    pages 12f: model
%    pages 21ff: walktrough of a solution
%
% With 7 tourist sites and 7 judges:
% """
% Every tourist site is visited by r = 3 judges.
% Every judge visits c = 3 tourist sites.
% Every pair of sites is visited by lambda = 1 common judge.
% """
%
% There are 151200 solutions to this problem.
% With the additional constraint that Ali should visit Birka, Falun and Lund
% there are 4320 solutions.
%
%
% This problem was also presented as "The Airline-of-the-Year Problem"
% in his (Flener's) presentation
% "Constraint Programming - Programming Paradigm on the Rise"
% http://www.it.uu.se/research/group/astra/ATM-CT/Flener.pdf
% page 4f
% The problem is stated as follows for 7 airlines and 7 judges:
% """
% Constant jury: Every airline is tested by 3 judges.
% Constant load: Every judge tests 3 airlines.
% Equity: Every airline pair is tested by 1 common judge.
% """
%
% Also, see the Comet model
%   http://www.hakank.org/comet/tourist_site_competition.co  
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: r = 3;
int: c = 3;
int: lambda = 1;

int: Birka = 1;
int: Falun = 2;
int: Lund = 3;
int: Mora = 4;
int: Sigtuna = 5;
int: Uppsala = 6;
int: Ystad = 7;
int: num_sites = 7;
array[1..num_sites] of 1..num_sites: sites = [Birka, Falun, Lund, Mora, Sigtuna, Uppsala, Ystad];

int: Ali = 1;
int: Dan = 2;
int: Eva = 3;
int: Jim = 4;
int: Leo = 5;
int: Mia = 6;
int: Ulla = 7;
int: num_judges = 7;
array[1..num_judges] of 1..num_judges: judges = [Ali, Dan, Eva, Jim, Leo, Mia, Ulla];

% decision variable
array[1..num_sites, 1..num_judges] of var 0..1: x;
% aux. variable
array[1..num_judges] of var set of 1..num_sites: judges_where;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % Symmetry breaking
  forall(s in {Birka, Falun, Lund}) (
    x[s, Ali] = 1
  )

  %/\ % Symmetry breaking (with general representation)
  % forall(s in 1..3) (
  %  x[s, 1] = 1
  %)

  /\ % Every tourist site is visited by r judges.
  forall(s in sites) (
    r = sum(j in judges) (x[s,j])
  )

  /\ % Every judge visits c tourist sites.
  forall(j in judges) (
    c = sum(s in sites) (x[s,j])
  )

  /\ % Every pair of sites is visited by lambda common judge.
  forall(s1 in sites, s2 in sites where s1 < s2) (
    lambda = sum(j in judges) (bool2int(x[s1,j] = 1 /\ x[s1,j] = x[s2,j])) 
  )

  /\ % where are the judges?
  forall(j in judges) (
    forall(s in sites) (
       x[s,j] == 1 <-> s in judges_where[j] 
    )
  )
;


output [
  if j = 1 then "\n" else " " endif ++
   show(x[i,j])
  | i in 1..num_sites, j in 1..num_judges
]
++
[
  "\njudges_where: " ++ show(judges_where)
]
++
[ 
  "\nx: " ++ show(x)
]
;
