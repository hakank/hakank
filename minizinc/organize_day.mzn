%
% Operation Research problem in Minizinc
% Organizing a day.
% 
% Problem formulation:
% Slides on (finite domain) Constraint Logic Programming, page 38f
%
% http://www.icparc.ic.ac.uk/eclipse/reports/eclipse.ppt
% (via http://www.icparc.ic.ac.uk/eclipse/reports/ )
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% The tasks are: [Work, Mail, Shop, Bank]
int: n; 
array[1..n] of var 9..17: Begins;
array[1..n] of var 9..17: Ends;
array[1..n] of 0..24: Durations;

% task[i,1] must be finished before task[i,2]
array[1..2, 1..2] of int: BeforeTasks;

% No overlapping
predicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) =
    s1 + d1 <= s2 \/ s2 + d2 <= s1;

% general constraints
constraint 
        forall(i in 1..n) (Ends[i] = Begins[i] + Durations[i]) /\
        forall(i,j in 1..n where i < j) (
              no_overlap(Begins[i], Durations[i], Begins[j], Durations[j])
        )
;

% specifika constraints
constraint
   forall(i in index_set_1of2(BeforeTasks)) (
      Ends[BeforeTasks[i,1]] <= Begins[BeforeTasks[i,2]]
   )
   /\
   Begins[1] >= 11 

;


solve satisfy;
% alternative objectives
% solve minimize sum(i in 1..n) (Begins[i]);
% solve minimize Begins[1];
% solve minimize Begins[2];
% solve minimize Begins[3];
% solve minimize Begins[4];

%
% data
%
n = 4;
Durations = [4,1,2,1];

% tasks that must be completed before some other task
BeforeTasks = 
[|4,3,
 |2,1
|];

output [
   "           [Work, Mail, Shop, Bank]\n", 
   "Begins:    ", show(Begins), "\n",
   "Durations: ", show(Durations), "\n",
   "Ends:      ", show(Ends), "\n",

]
