% 
% Congress puzzle in MiniZinc.
% 
% From the Mosel example problem k3congress_ka.mos
% """
% Congress puzzle
%
% At an international congress taking place on 7-11 August
% five researchers of different nationality are giving talks,
% each on a different day. Find out the date and nationality
% for every researcher from the following information:
% a) Michael is not Japanese.
% b) Eric is French and he talks before the 10th.
% c) Arabinda talks on the 9th.
% d) The Chinese who is not Hitoshi gives his talk on the 8th, 
%    before Michael.
% e) Hitoshi does his talk after the Indian and before the American.

% (c) 2005 Dash Associates
%     author: S. Heipcke, March 2005
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
set of int: DAYS = 7..11;

int: Arabinda = 1;
int: Eric     = 2;
int: Hitoshi  = 3;
int: Michael  = 4;
int: Zhicheng = 5;
array[1..n] of string: NAMES = ["Arabinda","Eric","Hitoshi","Michael","Zhicheng"];

int: Japanese = 1;
int: French   = 2;
int: Chinese  = 3;
int: American = 4;
int: Indian   = 5;
array[1..n] of string: NAT   = ["Japanese","French","Chinese","American","Indian"];

% decision variables

array[1..n] of var DAYS: talkp;  % Choice of day for person p
array[1..n] of var DAYS: talkn;  % Choice of day for nationality n


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  % Every researcher on a different day
  all_different(talkp)

  /\ 
  % Every nationality exactly once
  all_different(talkn)

  /\  
  % a: Michael is not Japanese
  talkp[Michael] != talkn[Japanese]

  /\
  % b: Eric is French and he talks before the 10th
  talkp[Eric] = talkn[French]
  /\ 
  talkp[Eric] <= 9

  /\
  % c: Arabinda talks on the 9th
  talkp[Arabinda] = 9

  /\
  % d: The Chinese who is not Hitoshi gives his talk on the 8th, before Michael
  talkp[Hitoshi] != talkn[Chinese]
  /\
  talkn[Chinese] = 8
  /\
  talkp[Michael] >= 9
  
  /\
  % e: Hitoshi does his talk after the Indian and before the American
  talkp[Hitoshi] >= talkn[Indian] + 1
  /\
  talkp[Hitoshi] <= talkn[American] - 1
;

output [
  "talkp: " ++ show(talkp) ++ "\n" ++
  "talkn: " ++ show(talkn) ++ "\n"
];

