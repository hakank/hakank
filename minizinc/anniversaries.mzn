% 
% Anniversaries problem in MiniZinc.
% 
% From http://www.genealogyworldwide.com/genealogy_fun.php
% (via http://l4f.cecs.anu.edu.au/puzzles/intermediate/anniversaries/)
% """
% On June 1st, five couples who live in Trumbull will celebrate their 
% wedding anniversaries. 
%
% Their surnames are 
%   Johnstone, Parker, Watson, Graves, and Shearer. 
% The husbands' given names are 
%   Russell, Douglas, Charles, Peter, and Everett. 
% The wives' given names are 
%   Elaine, Joyce, Marcia, Elizabeth, and Mildred. 
%
% Keep in mind that no two couples have been married the same number of years. 
% 
% From the clues given, try to determine the husband and wife that make up each 
% couple and the number of years they have been married.
%
% * Joyce has not been married as long as Charles or the Parkers,
%   but longer than Douglas or the Johnstones.
% * Elizabeth has been married twice as long as the Watsons, but only half as 
%   long as Russell.
% * The Shearers have been married ten years longer than Peter and ten 
%   years less than Marcia.
% * Douglas and Mildred have been married for 25 years less than the Graves who, 
%   having been married for 30 years, are the couple who have been married 
%   the longest.
% * Neither Elaine nor the Johnstones have been married the shortest amount of time.
% * Everett has been married for 25 years. 
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

% Their surnames are 
int: Johnstone = 1;
int: Parker = 2;
int: Watson = 3;
int: Graves = 4;
int: Shearer = 5;

array[1..n] of 1..n: surnames = [Johnstone, Parker, Watson, Graves, Shearer];
array[1..n] of string: surnames_s = ["Johnstone", "Parker", "Watson", "Graves", "Shearer"];

% The husbands' given names are 
%   Russell, Douglas, Charles, Peter, and Everett. 
var 1..n: Russell;
var 1..n: Douglas;
var 1..n: Charles;
var 1..n: Peter;
var 1..n: Everett;
array[1..n] of var 1..n: husbands = [Russell, Douglas, Charles, Peter, Everett];
array[1..n] of string: husbands_s = ["Russell", "Douglas", "Charles", "Peter", "Everett"];

% The wives' given names are 
%   Elaine, Joyce, Marcia, Elizabeth, and Mildred. 
var 1..n: Elaine;
var 1..n: Joyce;
var 1..n: Marcia;
var 1..n: Elizabeth;
var 1..n: Mildred;
array[1..n] of var 1..n: wifes = [Elaine, Joyce, Marcia, Elizabeth, Mildred];
array[1..n] of string: wifes_s = ["Elaine", "Joyce", "Marcia", "Elizabeth", "Mildred"];

% how long have the couple been married?
array[1..n] of var 1..30: married; 


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(husbands) /\
  all_different(wifes)    /\

  % Keep in mind that no two couples have been married the same 
  % number of years. 
  all_different(married)  /\

  % * Joyce has not been married as long as Charles or the Parkers,
  %   but longer than Douglas or the Johnstones.
  married[surnames[Joyce]] < married[surnames[Charles]] /\
  married[surnames[Joyce]] < married[Parker] /\
  married[surnames[Joyce]] > married[surnames[Douglas]] /\
  married[surnames[Joyce]] > married[Johnstone] /\

  % * Elizabeth has been married twice as long as the Watsons, but only half as 
  %   long as Russell.
  married[surnames[Elizabeth]]     = 2 * married[Watson] /\
  married[surnames[Elizabeth]] * 2 =     married[surnames[Russell]] /\


  % * The Shearers have been married ten years longer than Peter and ten 
  %   years less than Marcia.
  married[Shearer]      = 10 + married[surnames[Peter]] /\
  married[Shearer] + 10 = married[surnames[Marcia]] /\


  % * Douglas and Mildred have been married for 25 years less than the Graves who, 
  %   having been married for 30 years, are the couple who have been married 
  %   the longest.
  Douglas = Mildred /\
  married[surnames[Douglas]] + 25 = married[Graves] /\
  married[Graves] = 30 /\

  max(married) = married[Graves] /\

  % * Neither Elaine nor the Johnstones have been married the shortest amount of time.
  married[surnames[Elaine]] > min(married) /\
  married[Johnstone] > min(married) /\

  % * Everett has been married for 25 years. 
  married[surnames[Everett]] = 25

;

output [
  % "surnames: ", show(surnames), "\n",
  "husbands: ", show(husbands), "\n",
  "wifes   : ", show(wifes), "\n",
  "married : ", show(married), "\n",
]
++
[
  show(surnames_s[s]) ++ ": " ++
    show(join("",[husbands_s[p] | p in 1..n where s = fix(husbands[p])])) ++ " and " ++ 
    show(join("",[wifes_s[p] | p in 1..n where s = fix(wifes[p])])) ++ " " ++ 
    show(married[s]) ++ " years\n"
  | s in 1..n
]
;

