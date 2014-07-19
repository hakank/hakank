% 
% Fantasy OR in MiniZinc.
% 
% Problem and XPress Mosel model from
% Martin J Chlond "Fantasy OR"
% http://archive.ite.journal.informs.org/Vol4No3/Chlond/index.php 
% """
% An interesting article by J.R. Partington investigating puzzles of a mathematical 
% nature to be found embedded within computer fantasy games may be found at 
% Mathematical Puzzles in Fantasy Games. 
% [http://www1.maths.leeds.ac.uk/~pmt6jrp/personal/tms.html ]
% The following is an example of the whimsical delights to be found therein. 
%
%    A Scheduling Problem from Sangraal
%
%    When the Sangraal (Holy Grail) is almost won you arrive at the castle 
%    where the Foul Fiend has imprisoned 8 knights. These are as follows:
%
%    Agravain - lightly bound - badly wounded
%    Bors - lightly bound - scratched
%    Caradoc - bound a bit more - badly wounded
%    Dagonet - bound as C - scratched
%    Ector - bound and gagged - somewhat wounded
%    Feirefiz - in chains - badly wounded
%    Gareth - in chains and gagged - somewhat wounded
%    Harry - bound really tight in chains (poor chap) - scratched
%
%    Here the state of binding means that it will take 1, 1, 2, 2, 3, 4, 5 and 6 minutes 
%    (respectively) to free them: a freed knight then goes away to wash and recover himself 
%    physically in time for the Sangraal's arrival. The time he takes for this second stage 
%    is 5, 10 or 15 minutes, according to injury. In twenty minutes' time the sun will 
%    set and the Sangraal will arrive. How many knights can you bring? We see, for example 
%    that if you want F, you must free him almost at once, as he can only be ready in 
%    19 minutes at the earliest. Freeing Harry, though it takes 6 minutes, is not urgent, 
%    as he only needs to be freed by the 15th minute. 
% """  

%
% This is CP version using cumulative/4.
% Cf the IP model http://hakank.org/minizinc/sangraal.mzn
%
% There are 192 optimal solutions (6 rescued knights), all has
% the same rescued knigths:
%   rescued: [1, 1, 1, 1, 1, 0, 0, 1]
%
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: k = 8;

array[1..k] of int: free; % time to free each knight
array[1..k] of int: prep; % time to prepare each knight
int: time_limit;

array[1..k] of var 0..20: start; % start times
array[1..k] of var 0..100: end; % end times
array[1..k] of var 0..1: rescued; % which knigth to be rescued
var 0..k: z = sum(rescued); % number of knights rescused

solve :: int_search(start ++ rescued, first_fail, indomain_split, complete) maximize z;
% solve :: int_search(start ++ rescued, first_fail, indomain_split, complete) satisfy;

constraint  

  forall(i in 1..k) (
     end[i] = start[i] + free[i] + prep[i] 
     /\
     (end[i] <= time_limit <-> rescued[i] = 1)
  )
  /\
  cumulative(start,free,[1 | i in 1..k], 1)
 
  % /\ z = 6 
;

output 
[
  "start  : ", show(start), "\n",
  "free   : ", show(free), "\n",
  "prep   : ", show(prep), "\n",
  "end    : ", show(end), "\n",
  "rescued: ", show(rescued), "\n",
  "z      : ", show(z),"\n"
];

%
% data
%
free = [1, 1, 2,2, 3, 4, 5,6];
prep = [15,5,15,5,10,15,10,5];
time_limit = 20;
