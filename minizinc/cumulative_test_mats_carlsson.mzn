% 
% Test of global constraint cumulative in MiniZinc.
%
% cumulative is defined in globals.mzn (in the MiniZinc package).
%  
% From Mats Carlsson: "Trolleri med villkorsprogrammering" (swedish)
% (Magic with constraint programming)
% ftp://ftp.sics.se/pub/isl/papers/FD_sw.pdf
%

% from globals.mzn:
% """
% Requires that a set of tasks given by start times 's', durations 'd', and
% resource requirements 'r', never require more than a global resource bound
% 'b' at any one time.
% Assumptions:
% - forall i, d[i] >= 0 and r[i] >= 0
%-----------------------------------------------------------------------------%
%predicate cumulative(array[int] of var int: s,  % start 
%                     array[int] of var int: d,  % duration
%                     array[int] of var int: r,  % resource 
%                     var int: b)                % resource bound
%
% """

% In Carlssons presentation it's presented as
%     cumulative(Origins,Durations,Heights,Limit)
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% This is from Carlsson's presentation
var 1..2: O1; % start task 1
var 2..3: D1; % duration task 1
var 1..4: H1; % height task 1
var 3..3: O2; % start task 2
var 2..2: D2; % duration task 2
var 2..3: H2; % height task 2

% test:
% var 1..4: O1; % start task 1
% var 1..4: D1; % duration task 1
% var 1..4: H1; % height task 1
% var 1..4: O2; % start task 2
% var 1..4: D2; % duration task 2
% var 1..4: H2; % height task 2


% solve satisfy;
solve :: int_search([O1,D1,H1,O2,D2,H2], first_fail, indomain_min, complete) satisfy;

constraint
 
        cumulative([O1,O2],[D1,D2],[H1,H2],4)

;

output [

  "task 1: ", show([O1,D1,H1]), " task 2: ", show([O2,D2,H2]), "\n",
];
