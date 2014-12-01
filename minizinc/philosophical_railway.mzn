% 
% Muddle Management problem in MiniZinc.
% 
% From 
% http://logic4fun.rsise.anu.edu.au/demo/example1.html
% """
% Philosophical Railway
% It happened, in the days when trains used to call at the tiny village of 
% Much Tittering in the Woods, that the 1215 once pulled up there and stood 
% for the best part of an hour. Nobody now remembers why. At any rate, the 
% driver, the porter, the signalman, the stationmaster and the guard spent the 
% time in such merry conversation as is customary among employees of railway 
% companies. Their names, in alphabetical order, were 
%   James, Kant, Locke, Mill and Nietzsche. 
% For reasons lost in the mists of railway history, they agreed to make two 
% statements each, one true and the other false. They said:
%
% MILL: 	Nietzsche is the stationmaster.
%               James is either the guard or the porter.
% LOCKE: 	Neither Kant nor Nietzsche is the signalman.
%               Mill is not the stationmaster.
% KANT: 	Mill's second statement was false.
%               Locke's first statement was true.
% NIETZSCHE: 	Either James is the porter or I am.
%               Neither Locke nor Mill is the guard.
% JAMES: 	I am not the signalman.
%               Nietzsche's second statement was false.
% 
% What was the driver's name? 
% """
%
% Encoding in Finder:
% http://logic4fun.rsise.anu.edu.au/demo/encoding1.html

% Solution:
% http://logic4fun.rsise.anu.edu.au/cgi/wwfinder/puzzledemo1
% """
% Model 1
% 
% philosopher | James     Kant      Locke     Mill      Nietzsche
% ------------+--------------------------------------------------
%         job | guard     porter    signalman s'master  driver
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

int: driver = 1;
int: guard = 2;
int: porter = 3;
int: signalman = 4;
int: stationmaster = 5;
array[1..n] of int: occupation = [driver, guard, porter, signalman, stationmaster];
array[1..n] of string:  occupation_s = ["driver", "guard", "porter", "signalman", "stationmaster"];

% decision variables
var 1..n: James;
var 1..n: Kant;
var 1..n: Locke;
var 1..n: Mill;
var 1..n: Nietzsche;
array[1..n] of var 1..n:  philosophers = [James, Kant, Locke, Mill, Nietzsche];
array[1..n] of string:  philosophers_s = ["James", "Kant", "Locke", "Mill", "Nietzsche"];

solve satisfy;
% solve :: int_search(philosophers, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(philosophers)   /\

% MILL: 	Nietzsche is the stationmaster.
%               James is either the guard or the porter.
  (
    bool2int(Nietzsche = stationmaster)  
    +
    bool2int(James = guard \/ James = porter)
    = 1
  )
  
% LOCKE: 	Neither Kant nor Nietzsche is the signalman.
%               Mill is not the stationmaster.
  /\
  ( 
     bool2int(Kant != signalman /\ Nietzsche != signalman)
     +
     bool2int(Mill != stationmaster)
     = 1
  )


% KANT: 	Mill's second statement was false.
%               Locke's first statement was true.
  /\
  (
    bool2int(not(James = guard \/ James = porter)) % Mill's second
    +
    bool2int(Kant != signalman /\ Nietzsche != signalman) % Locke's first)
    = 1
  )

% NIETZSCHE: 	Either James is the porter or I am.
%               Neither Locke nor Mill is the guard.
  /\
  (
     bool2int(James = porter \/ Nietzsche = porter)
     +
     bool2int(Locke != guard /\ Mill != guard)
     = 1
  )

% JAMES: 	I am not the signalman.
%               Nietzsche's second statement was false.
  /\
  (
     bool2int(James != signalman)  
     +
     bool2int(not(Locke != guard /\ Mill != guard))       % Nietzsche's second
     = 1
  )
;

output [
  show(philosophers), "\n",
]
++
[
 show(philosophers_s[i]) ++ ": " ++ show(occupation_s[fix(philosophers[i])]) ++ "\n"
 | i in 1..n
]
;

