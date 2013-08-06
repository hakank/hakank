% 
% Jobs Puzzle in MiniZinc.
%
% (This is a standard problem in Automatic Reasoning.)
%
%
% From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
% """
% Jobs Puzzle
% 
% There are four people:  Roberta, Thelma, Steve, and Pete.
%  Among them, they hold eight different jobs.
%  Each holds exactly two jobs.
%  The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
%  teacher, actor, and boxer.
%  The job of nurse is held by a male.
%  The husband of the chef is the clerk.
%  Roberta is not a boxer.
%  Pete has no education past the ninth grade.
%  Roberta, the chef, and the police officer went golfing together.
%
%  Question:  Who holds which jobs?
%
%
% The answer:
% Chef       Thelma
% Guard      Roberta
% Nurse      Steve
% Clerk      Pete
% Police     Steve
% Teacher    Roberta
% Actor      Pete
% Boxer      Thelma

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com .
% See also my MiniZinc page: http://www.hakank.org/minizinc .
%
include "globals.mzn";

set of int: p = 1..4;

p: Roberta = 1;
p: Thelma = 2;
p: Steve = 3;
p: Pete = 4;

array[1..4] of var p: Persons = [Roberta, Thelma, Steve, Pete];

set of int: job = 1..4;
var job: chef;
var job: guard;
var job: nurse;
var job: clerk;
var job: police_officer;
var job: teacher;
var job: actor;
var job: boxer;

array[1..8] of var job: Jobs = [chef, guard, nurse, clerk, police_officer, teacher, actor, boxer];

solve satisfy;

constraint
    %  Each holds exactly two jobs.
    global_cardinality(Jobs,set2array(index_set([2,2,2,2])), [2,2,2,2]) /\

    %  The job of nurse is held by a male.
    (nurse = Steve \/ nurse = Pete) /\

    %  The husband of the chef is the clerk.
    (clerk = Steve \/ clerk = Pete) /\
    (chef = Roberta \/ chef = Thelma) /\
    chef != clerk /\

    %  Roberta is not a boxer.
    Roberta != boxer /\

    %  Pete has no education past the ninth grade.
    Pete != teacher /\ Pete != police_officer /\ Pete != nurse /\

    % Roberta, [and] the chef, and the police officer went golfing together.
    Roberta != chef /\ 
    chef    != police_officer /\
    Roberta != police_officer /\

     % From the name of the job
    (actor = Steve \/ actor = Pete)
;

output [
   "Roberta:1 Thelma:2 Steve:3 Pete:4\n",
   "[chef, guard, nurse, clerk, police_officer, teacher, actor, boxer]\n",
   show(Jobs),"\n"

]
