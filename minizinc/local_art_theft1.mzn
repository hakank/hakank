% 
% Local art theft problem in MiniZinc.
% 
% From Stack Overflow:
% "Local theft art in prolog program"
% http://stackoverflow.com/questions/16862293/local-theft-art-in-prolog-program
% """
% I have to write program in prolog to solve this logic problem:
%
% After a local art theft, six suspects were being interviewed. Below is a summary 
% of their statements. Police know that exactly four of them told one lie each and all 
% of the other statements are true. From this information can you tell who committed 
% the crime?
%
% Alan said:
% It wasn't Brian It wasn't Dave It wasn't Eddie
%
% Brian said:
% It wasn't Alan It wasn't Charlie It wasn't Eddie
%
% Charlie said:
% It wasn't Brian It wasn't Freddie It wasn't Eddie
%
% Dave said:
% It wasn't Alan It wasn't Freddie It wasn't Charlie
%
% Eddie said:
% It wasn't Charlie It wasn't Dave It wasn't Freddie
%
% Freddie said:
% It wasn't Charlie It wasn't Dave It wasn't Alan
% ...
% """
% 

% This model is very verbose, much since MiniZinc don't automatically
% convert between bool <-> 0..1. 
% For a much neater model, see 
%   http://www.hakank.org/minizinc/local_art_theft.mzn


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% truth values
var bool: Alan;
var bool: Brian;
var bool: Charlie;
var bool: Dave;
var bool: Eddie;
var bool: Freddie;

% counts
var 2..3: AlanC;
var 2..3: BrianC;
var 2..3: CharlieC;
var 2..3: DaveC;
var 2..3: EddieC;
var 2..3: FreddieC;

array[1..6] of string: s = ["Alan", "Brian", "Charlie", "Dave", "Eddie", "Freddie"];

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

  % Alan said: It wasn't Brian It wasn't Dave It wasn't Eddie
  (Alan -> (not(Brian) /\ not(Dave) /\ not(Eddie)))

  % Brian said: It wasn't Alan It wasn't Charlie It wasn't Eddie
  /\
  (Brian -> (not(Alan) /\ not(Charlie) /\ not(Eddie)))

  % Charlie said: It wasn't Brian It wasn't Freddie It wasn't Eddie
  /\
  (Charlie -> (not(Brian) /\ not(Freddie) /\ not(Eddie)))

  % Dave said: It wasn't Alan It wasn't Freddie It wasn't Charlie
  /\
  (Dave -> (not(Alan) /\ not(Freddie) /\ not(Charlie)))

  % Eddie said: It wasn't Charlie It wasn't Dave It wasn't Freddie
  /\
  (Eddie -> (not(Charlie) /\ not(Dave) /\ not(Freddie)))

  % Freddie said: It wasn't Charlie It wasn't Dave It wasn't Alan
  /\
  (Freddie -> (not(Charlie) /\ not(Dave) /\ not(Alan)))

  /\
  % Police know that exactly four of them told one lie each and all 
  % of the other statements are true. 
  (AlanC = bool2int(not(Brian)) + bool2int(not(Dave)) + bool2int(not(Eddie)))

  % Brian said: It wasn't Alan It wasn't Charlie It wasn't Eddie
  /\
  (BrianC = bool2int(not(Alan)) + bool2int(not(Charlie)) + bool2int(not(Eddie)))

  % Charlie said: It wasn't Brian It wasn't Freddie It wasn't Eddie
  /\
  (CharlieC = bool2int(not(Brian)) + bool2int(not(Freddie)) + bool2int(not(Eddie)))

  % Dave said: It wasn't Alan It wasn't Freddie It wasn't Charlie
  /\
  (DaveC = bool2int(not(Alan)) + bool2int(not(Freddie)) + bool2int(not(Charlie)))

  % Eddie said: It wasn't Charlie It wasn't Dave It wasn't Freddie
  /\
  (EddieC = bool2int(not(Charlie)) + bool2int(not(Dave)) + bool2int(not(Freddie)))

  % Freddie said: It wasn't Charlie It wasn't Dave It wasn't Alan
  /\
  (FreddieC = bool2int(not(Charlie)) + bool2int(not(Dave)) + bool2int(not(Alan)))
  /\
  count([AlanC,BrianC,CharlieC, DaveC, EddieC, FreddieC], 2, 4)

  % From this information can you tell who committed 
  % the crime?  

;


output [
  show([Alan,Brian,Charlie,Dave, Eddie, Freddie]) ++ "\n" ++
  show([AlanC,BrianC,CharlieC,DaveC,EddieC,FreddieC]) ++ "\n"
]
++
[
  if fix(Alan) = true then "Alan" elseif 
     fix(Brian) = true then "Brian" elseif
     fix(Charlie) = true then "Charlie" elseif
     fix(Dave) = true then "Dave" elseif
     fix(Eddie) = true then "Eddie" elseif
     fix(Freddie) = true then "Freddie" else
     "Nobody"
   endif ++ " committed the crime."
]
;
