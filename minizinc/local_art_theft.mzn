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

% For a much more verbose model, see 
%   http://www.hakank.org/minizinc/local_art_theft1.mzn


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6;
array[1..n, 1..n] of 0..1: noblame = array2d(1..n, 1..n, [
% A B C D E F 
  0,1,0,1,1,0, % Alan said ..
  1,0,1,0,1,0, % Brian
  0,1,0,0,1,1, % Charlie
  1,0,1,0,0,1, % Dave
  0,0,1,1,0,1, % Eddie
  1,0,1,1,0,0, % Freddie
]);

array[1..n] of string: s = ["Alan", "Brian", "Charlie", "Dave", "Eddie", "Freddie"];

% decision variables
array[1..n] of var bool: b;
array[1..n] of var 2..3: counts;


solve satisfy;
% solve :: int_search(counts, "first_fail", "indomain", "complete") satisfy;

constraint

  forall(i in 1..n) (
     (b[i] -> forall(j in 1..n where noblame[i,j] = 1) ( not(b[j]) )) 
     /\
     counts[i] = sum(j in 1..n where noblame[i,j] = 1) ( bool2int(not(b[j])))
  )

  /\ count(counts, 2, 4)
  % /\
  % sum(i in 1..n) (bool2int(b[i])) = 1 % not needed

  % From this information can you tell who committed 
  % the crime?  

;


output [
  "b     : " ++ show(b) ++ "\n" ++ 
  "counts: " ++ show(counts) ++ "\n"
]
++
[
  if fix(b[i]) = true then show(s[i]) ++ " committed the crime.\n" else "" endif
  | i in 1..n
]
;
