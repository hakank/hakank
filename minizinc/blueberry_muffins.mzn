% 
% Blueberry Muffins problem in MiniZinc.
% 
% Problem description from 
% http://brownbuffalo.sourceforge.net/BlueberryMuffinsClues.html
% """
% Daniel made a dozen blueberry muffins on Friday night -- and by
% the time he was ready for brunch on Saturday, there were only
% two left. The other ten had been snitched by his housemates,
% all of whom had gotten up early because they had to work on
% Saturday. The four housemates include two men named Bill and
% Mark, and two women named Calla and Lynn; last names are Ellis,
% Ingham, Oakley, and Summers, and their differing professions
% are dogcatcher, flautist, secretary, and zookeeper. Can you
% discover each one's full name, profession, and number of
% muffins snitched?
%
% 1. Each housemate snitched a different number of muffins from one to four.
% 2. Bill and Ellis snitched a total of six muffins.
% 3. The secretary (who is a woman) snitched more than the dogcatcher.
% 4. Mark snitched two more than Summers did.
% 5. The flautist snitched twice as many as Ms. Oakley did.
% 6. Calla's last name isn't Ingham.
% 
% Determine: First name -- Last name -- Job -- Muffins
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 4;

int: Bill = 1;
int: Mark = 2;
int: Calla = 3;
int: Lynn = 4;

array[1..n] of string: firstname_s = ["Bill", "Mark", "Calla", "Lynn"];

var 1..n: Ellis;
var 1..n: Ingham;
var 1..n: Oakley;
var 1..n: Summers;
array[1..n] of var 1..n: lastname = [Ellis, Ingham, Oakley, Summers];
array[1..n] of string: lastname_s = ["Ellis", "Ingham", "Oakley", "Summers"];

var 1..n: dogcatcher;
var 1..n: flautist;
var 1..n: secretary;
var 1..n: zookeeper;
array[1..n] of var 1..n: job = [dogcatcher, flautist, secretary, zookeeper];
array[1..n] of string: job_s = ["dogcatcher", "flautist", "secretary", "zookeeper"];

int: num_muffins = 10;
array[1..n] of var 1..4: muffins;


solve satisfy;
% solve :: int_search(muffins ++ lastname ++ job, first_fail, indomain_min, complete) satisfy;


constraint
   alldifferent(lastname) /\
   alldifferent(job)
; 

constraint
   % 1. Each housemate snitched a different number of muffins from one to four.
   alldifferent(muffins)

   % 2. Bill and Ellis snitched a total of six muffins.
   /\
   muffins[Bill] + muffins[Ellis] = 6 
   /\
   Ellis != Bill

   % 3. The secretary (who is a woman) snitched more than the dogcatcher.
   /\
   (secretary = Calla \/ secretary = Lynn)
   /\
   muffins[secretary] > muffins[dogcatcher]

   % 4. Mark snitched two more than Summers did.
   /\
   muffins[Mark] = muffins[Summers] + 2
   /\
   Summers != Mark

   % 5. The flautist snitched twice as many as Ms. Oakley did.
   /\
   muffins[flautist] = muffins[Oakley] * 2
   /\
   flautist != Oakley
   /\
   (Oakley = Calla \/ Oakley = Lynn)

   % 6. Calla's last name isn't Ingham.
   /\
   Ingham != Calla
;

output [
  "lastname: " ++ show(lastname) ++ "\n" ++
  "job     : " ++ show(job)      ++ "\n" ++ 
  "muffins : " ++ show(muffins)  ++ "\n\n" 
] ++ 
[
  % a nicer presentation...
  let {
    var 1..n: lastname_ix = max([j*bool2int(lastname[j] = i) | j in 1..n]),
    var 1..n: job_ix =  max([j*bool2int(job[j] = i) | j in 1..n])
  } in
  firstname_s[i] ++ " " ++
  lastname_s[fix(lastname_ix)] ++  ", " ++
  job_s[fix(job_ix)] ++  ": " ++
  show(muffins[i]) ++ " muffins\n" 
 | i in 1..n
] ++
["\n"];