%
% Constraint Programming: Moving furnitures in Minizinc
% 
% Problem from Marriott & Stuckey: 'Programming with constraints', page  112f
%
% Cf http://www.hakank.org/constraints/FurnitureMoving.java

%
% This Minizinc program is written by Hakan Kjellerstrand and is commented in 
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
%
% One solution of the minimization problem:
% NumPersons: 3
% piano: 30 
% chair: 15
% bed: 0
% table: 15
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n; % number of things
int: upperLimit;
array[1..n] of var 0..upperLimit: Starts;
array[1..n] of var 0..upperLimit: Durations;
array[1..n] of var 0..upperLimit: Resources;
array[1..n] of var 0..upperLimit*2: EndTimes;
var 0..100: numPersons; 
var 0..100: end_time;

% solve satisfy;             % just give a solution (combined with the extra constraints)
% solve minimize numPersons;    % minimize the number of persons
solve minimize end_time;

constraint
    % defined in globals.mzn
    cumulative(Starts, Durations, Resources , numPersons)

   % setting EndTimes
   /\ forall(i in 1..n) (EndTimes[i] = Starts[i] + Durations[i])

   /\
   end_time = max(EndTimes)

   %
   % Some extra constraint to play with:
   % 
   %  How many persons if everything should start at the same time
   %    /\ forall(i in 1..n) (Starts[i] = 0)   

   % Must be finished in 60 minutes
   % /\ forall(i in 1..n) (Starts[i] + Durations[i] <= 60) 

   % limitation of the number of people
   /\ numPersons <= 4

   % must start on an even 10 minutes 
   % (works with fz)
   % /\  forall(i in 1..n) (Starts[i] mod 10 = 0)
;   


% data

upperLimit = 160;

n = 4;
Durations = [30,10,15,15];
Resources = [3,1,3,2];


output [
       "numPersons: ", show(numPersons),"\n",
       "Resources: ", show(Resources), "\n",
       "Starts:    ", show(Starts), "\n",
       "Durations: ", show(Durations),"\n",
       "EndTimes:  ", show(EndTimes),"\n",
       "end_time:  ", show(end_time),"\n"
]


