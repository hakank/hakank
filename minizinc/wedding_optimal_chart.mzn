% 
% Finding an optimal wedding seating chart in MiniZinc.
% 
% From 
% Meghan L. Bellows and J. D. Luc Peterson
% "Finding an optimal seating chart for a wedding"
% http://www.improbable.com/news/2012/Optimal-seating-chart.pdf
% http://www.improbable.com/2012/02/12/finding-an-optimal-seating-chart-for-a-wedding
% 
% """
% Every year, millions of brides (not to mention their mothers, future 
% mothers-in-law, and occasionally grooms) struggle with one of the 
% most daunting tasks during the wedding-planning process: the 
% seating chart. The guest responses are in, banquet hall is booked, 
% menu choices have been made. You think the hard parts are over, 
% but you have yet to embark upon the biggest headache of them all. 
% In order to make this process easier, we present a mathematical 
% formulation that models the seating chart problem. This model can 
% be solved to find the optimal arrangement of guests at tables. 
% At the very least, it can provide a starting point and hopefully 
% minimize stress and arguments… 
% """
%
%
%
% Here's one optimal solution with z = 276
% (and with symmetry breaking that Martha should sit at table 1):
% 
%   z: 276
%   tables:[2, 2, 1, 1, 2, 2, 3, 3, 1, 4, 4, 4, 4, 5, 5, 5, 5]
%   1: Deb        mother of the bride. Table 2
%   2: John       father of the bride. Table 2
%   3: Martha     sister of the bride. Table 1
%   4: Travis     boyfriend of Martha. Table 1
%   5: Allan      grandfather of the bride. Table 2
%   6: Lois       wife of Allan. Table 2
%   7: Jayne      aunt of the bride. Table 3
%   8: Brad       uncle of the bride. Table 3
%   9: Abby       cousin of the bride. Table 1
%  10: Mary Helen mother of the groom. Table 4
%  11: Lee        father of the groom. Table 4
%  12: Annika     sister of the groom. Table 4
%  13: Carl       brother of the groom. Table 4
%  14: Colin      brother of the groom. Table 5
%  15: Shirley    grandmother of the groom. Table 5
%  16: DeAnn      aunt of the groom. Table 5
%  17: Lori       aunt of the groom. Table 5
%
%  Table 1: Martha (B) Travis (B) Abby (B) 
%  Table 2: Deb (B) John (B) Allan (B) Lois (B) 
%  Table 3: Jayne (B) Brad (B) 
%  Table 4: Mary Helen (G) Lee (G) Annika (G) Carl (G) 
%  Table 5: Colin (G) Shirley (G) DeAnn (G) Lori (G) 
%
% There are 1080 solutions with z = 276 (where Martha sits at table 1)
%
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: a; % maximum number of guests a table can seat
int: b; % minimum number of people each guest knows at their table
int: n; % max number of tables
int: m; % maximum number of quests


% C[j,k]: Connection matrix, indicating relation of guest j to
%         guest k (0..50 where 0 is no relation, 50 is strong relation)
array[1..m, 1..m] of int: C;
array[1..m] of var 1..n: tables;

var 0..sum([C[j,k] | j,k in 1..m]): z; % to maximize

% solve maximize z;
% ann: var_select;
% ann: val_select;
solve :: int_search(
        tables,
        max_regret, % first_fail, 
        indomain_split, 
        complete) 
    maximize z;
    % minimize z;
    % satisfy;

% constraint z = 276; % for solve satisfy

% optimal value
constraint
   z = sum(j,k in 1..m where j < k) (
          C[j,k]*bool2int(tables[j]=tables[k])
       )
;

constraint
   forall(i in 1..n) (
     sum(j,k in 1..m where j < k) (bool2int(C[j,k] > 0 /\ 
                                            tables[j] = i /\ 
                                            tables[k] = i)) >= b
     /\
     sum(j in 1..m) (bool2int(tables[j] = i)) <= a
   )
   
   /\ % symmetry breaking
   % tables[3] = 1 % Martha sits at table 1
   tables[1] = 1
   
;

output [
  "z: " ++ show(z) ++ "\n"
]
++
["\ntables:" ++ show(tables) ++ "\n"]
++
[ 
  show(j) ++ ": " ++ show(names[j]) ++ ". Table " ++ show(tables[j]) ++ "\n"
  | j in 1..m
]
++
[
  if j = 1 then "\nTable " ++ show(i) ++ ": " else "" endif ++
    if fix(tables[j]) = i then
     show(names2[j]) ++ " " 
    else 
     ""
    endif
  | i in 1..n, j in 1..m
]
++ ["\n"]
;


%
% Data
%
m = 17; % maximum number of quests


%  j   Guest         Relation
%  -------------------------------------
%  1   Deb           mother of the bride
%  2   John          father of the bride
%  3   Martha        sister of the bride
%  4   Travis        boyfriend of Martha
%  5   Allan         grandfather of the bride
%  6   Lois          wife of Allan
%  7   Jayne         aunt of the bride
%  8   Brad          uncle of the bride
%  9   Abby          cousin of the bride
% 10   Mary Helen    mother of the groom
% 11   Lee           father of the groom
% 12   Annika        sister of the groom
% 13   Carl          brother of the groom
% 14   Colin         brother of the groom
% 15   Shirley       grandmother of the groom
% 16   DeAnn         aunt of the groom
% 17   Lori          aunt of the groom
%              Table 2: Guest List


C = array2d(1..m, 1..m,
[
 1,50, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
50, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1, 1,50, 1, 1, 1, 1,10, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1,50, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1, 1, 1, 1,50, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1, 1, 1,50, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1, 1, 1, 1, 1, 1,50, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1, 1, 1, 1, 1,50, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 1, 1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,50, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0,50, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1
]);


array[1..m] of string: names = [
 "Deb        mother of the bride",
 "John       father of the bride",
 "Martha     sister of the bride",
 "Travis     boyfriend of Martha",
 "Allan      grandfather of the bride",
 "Lois       wife of Allan",
 "Jayne      aunt of the bride",
 "Brad       uncle of the bride",
 "Abby       cousin of the bride",
 "Mary Helen mother of the groom",
 "Lee        father of the groom",
 "Annika     sister of the groom",
 "Carl       brother of the groom",
 "Colin      brother of the groom",
 "Shirley    grandmother of the groom",
 "DeAnn      aunt of the groom",
 "Lori       aunt of the groom",
];

array[1..m] of string: names2 = [
 "Deb (B)",
 "John (B)",
 "Martha (B)",
 "Travis (B)",
 "Allan (B)",
 "Lois (B)",
 "Jayne (B)",
 "Brad (B)",
 "Abby (B)",
 "Mary Helen (G)",
 "Lee (G)",
 "Annika (G)",
 "Carl (G)",
 "Colin (G)",
 "Shirley (G)",
 "DeAnn (G)",
 "Lori (G)",
];


n = 5; % max number of tables
a = 4; % maximum number of guests a table can seat
b = 2; % minimum number of people each guest knows at their table

% Easier problem
% n = 2; % max number of tables
% a = 10; % maximum number of guests a table can seat
% b = 1; % minimum number of people each guest knows at their table

