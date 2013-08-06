% 
% Secret Santa problem II in MiniZinc.
% 
% From Maple Primes: "Secret Santa Graph Theory"
% http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
% """
% Every year my extended family does a "secret santa" gift exchange. 
% Each person draws another person at random and then gets a gift for 
% them. At first, none of my siblings were married, and so the draw was 
% completely random. Then, as people got married, we added the restriction 
% that spouses should not draw each others names. This restriction meant 
% that we moved from using slips of paper on a hat to using a simple 
% computer program to choose names. Then people began to complain when 
% they would get the same person two years in a row, so the program was 
% modified to keep some history and avoid giving anyone a name in their 
% recent history. This year, not everyone was participating, and so after 
% removing names, and limiting the number of exclusions to four per person, 
% I had data something like this:
% 
% Name: Spouse, Recent Picks
% 
% Noah: Ava. Ella, Evan, Ryan, John
% Ava: Noah, Evan, Mia, John, Ryan
% Ryan: Mia, Ella, Ava, Lily, Evan
% Mia: Ryan, Ava, Ella, Lily, Evan
% Ella: John, Lily, Evan, Mia, Ava
% John: Ella, Noah, Lily, Ryan, Ava
% Lily: Evan, John, Mia, Ava, Ella
% Evan: Lily, Mia, John, Ryan, Noah
% """
% 
% Note: I interpret this as the following three constraints:
%   1) One cannot be a Secret Santa of one's spouse
%   2) One cannot be a Secret Santa for somebody two years in a row
%   3) Optimization: maximize the time since the last time 
%
% This model also handle single persons, something the original
% problem don't mention.
% 
%
% Compare with another Secret Santa problem:
% * http://www.hakank.org/minizinc/secret_santa.mzn
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Solution:
%
% This model gives the following 8 solutions, with total "distance" of 67:
% santa_distance: 9 9 4 9 9 9 9 9
% santas        : 7 5 8 6 1 4 3 2
%
% santa_distance: 9 9 4 9 9 9 9 9
% santas        : 7 5 8 6 3 4 1 2
%
% santa_distance: 9 9 9 4 9 9 9 9
% santas        : 7 5 6 8 1 4 3 2
% 
% santa_distance: 9 9 9 4 9 9 9 9
% santas        : 7 5 6 8 3 4 1 2
% 
% santa_distance: 9 9 9 9 4 9 9 9
% santas        : 4 7 1 6 2 8 3 5
% 
% santa_distance: 9 9 9 9 4 9 9 9
% santas        : 4 7 6 1 2 8 3 5
% 
% santa_distance: 9 9 9 9 9 9 4 9
% santas        : 4 7 1 6 3 8 5 2
% 
% santa_distance: 9 9 9 9 9 9 4 9
% santas        : 4 7 6 1 3 8 5 2
% 
% The best solution would be that all Santa
% relations is completely new.
% But as we can see by the santa_distance there
% is no such solution; there is alway one Santa
% with previous history. 
% This requirement was also tested, but is
% now commented.
%
% With the Single person (and some faked data) we can manage
% to get all brand new Secret Santas.
% 

include "globals.mzn"; 

int: n;       % number of persons
% "large" M used in rounds matrix to indicate no
% earlier history
int: M = n+1; 

array[1..n] of 0..8: spouses; % 0 for no spouse
array[1..n, 1..n] of 0..M: rounds; % Secret Santa matrix

array[1..n] of var 1..n: santas;
array[1..n] of var 1..n+1: santa_distance;
var int: z; % total of "distance" to maximize

% solve maximize z;
solve :: int_search(
        santa_distance ++ santas ++ [z], 
        first_fail, 
        indomain_min,
        complete)
    maximize z;
    % satisfy;

constraint
   all_different(santas)

   /\ % no Santa for a spouses
   forall(i in 1..n) (
      santas[i] != i 
      /\
      if spouses[i] > 0 then 
         santas[i] != spouses[i]
      else 
         true
      endif
   )
   /\ % optimize "distance" to earlier rounds:
   forall(i in 1..n) (
     santa_distance[i] = rounds[i,santas[i]]
   )
   /\
   % cannot be a Secret Santa for the same person two years in a row.
   forall(i in 1..n) (
      let { var 1..n: j } in
       rounds[i,j] = 1 /\ santas[i] != j
   )
   /\
   z = sum(santa_distance)

   % /\ z >= 67 % test for satisfy (original problem)

   %/\ % Test: must give to someone not in list. 
   %   % This don't work, see above.
   % forall(i in 1..n) (       
   %   rounds[i,santas[i]] > n
   % )
;

% n = 8;
n = 9; % With a Single person
int: Noah = 1;
int: Ava  = 2;
int: Ryan = 3;
int: Mia  = 4;
int: Ella = 5;
int: John = 6;
int: Lily = 7;
int: Evan = 8;
int: Single = 9;

spouses = [
    Ava,  % Noa
    Noah, % Ava
    Mia,  % Rya
    Ryan, % Mia
    John, % Ella
    Ella, % John
    Evan, % Lily
    Lily,  % Evan
    0     % Single has no spouse
]; 


%
% The matrix version of earlier rounds.
% M means that no earlier Santa.
% Note: Ryan and Mia has the same recipient for years 3 and 4,
%       and Ella and John has for year 4. 
%       This seems to be caused by modification of 
%       original data.
%
% rounds = array2d(1..n, 1..n, [
% %N  A  R  M  El J  L  Ev 
%  0, M, 3, M, 1, 4, M, 2, % Noah
%  M, 0, 4, 2, M, 3, M, 1, % Ava 
%  M, 2, 0, M, 1, M, 3, 4, % Ryan
%  M, 1, M, 0, 2, M, 3, 4, % Mia 
%  M, 4, M, 3, 0, M, 1, 2, % Ella
%  1, 4, 3, M, M, 0, 2, M, % John
%  M, 3, M, 2, 4, 1, 0, M, % Lily
%  4, M, 3, 1, M, 2, M, 0  % Evan
% ]);

%
% rounds with a single person (fake data)
%
rounds = array2d(1..n, 1..n, [
%N  A  R  M  El J  L  Ev S
 0, M, 3, M, 1, 4, M, 2, 2, % Noah
 M, 0, 4, 2, M, 3, M, 1, 1, % Ava 
 M, 2, 0, M, 1, M, 3, 4, 4, % Ryan
 M, 1, M, 0, 2, M, 3, 4, 3, % Mia 
 M, 4, M, 3, 0, M, 1, 2, M, % Ella
 1, 4, 3, M, M, 0, 2, M, M, % John
 M, 3, M, 2, 4, 1, 0, M, M, % Lily
 4, M, 3, 1, M, 2, M, 0, M, % Evan
 1, 2, 3, 4, M, 2, M, M, 0, % Single
]);


% Only for the minizinc solver
output [
  if i = 1 then "\n'Santa Distance': " ++ show(z) ++ "\n" else "" endif ++
  show(i) ++ " is a Secret Santa of " ++ show(santas[i]) ++ " dist: " ++ show(santa_distance[i]) ++ "\n"
  | i in 1..n
] ++ 
["\n"];
