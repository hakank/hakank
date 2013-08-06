/*

  Jobs puzzle in AMPL+CP.
  
  (This is a standard problem in Automatic Reasoning.)

  From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
  """
  Jobs Puzzle
  
  There are four people:  Roberta, Thelma, Steve, and Pete.
  Among them, they hold eight different jobs.
  Each holds exactly two jobs.
  The jobs are chef, guard, nurse, clerk, police officer (gender not implied),
  teacher, actor, and boxer.
  The job of nurse is held by a male.
  The husband of the chef is the clerk.
  Roberta is not a boxer.
  Pete has no education past the ninth grade.
  Roberta, the chef, and the police officer went golfing together.

  Question:  Who holds which jobs?
  """

  The answer:
  Chef       Thelma
  Guard      Roberta
  Nurse      Steve
  Clerk      Pete
  Police     Steve
  Teacher    Roberta
  Actor      Pete
  Boxer      Thelma
 

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

set Persons;
set Jobs;

var person{Persons} >= 1 <= n integer;
var jobs{Jobs} >= 1 <= n integer;

#
# constraints
#
# Fix the people
s.t. c0:
    person['Roberta'] = 1 and
    person['Thelma'] = 2 and
    person['Steve']  = 3 and
    person['Pete'] = 4
;

#  Each holds exactly two jobs.
s.t. c1{i in 1..n}:
   exactly 2 {j in Jobs} (jobs[j] = i)
;

#  The job of nurse is held by a male.
s.t. c2: 
   (jobs['nurse'] = person['Steve'] or jobs['nurse'] = person['Pete'])
;

#  The husband of the chef is the clerk.
s.t. c3: 
    (jobs['clerk'] = person['Steve'] or jobs['clerk'] = person['Pete']) and
    (jobs['chef'] = person['Roberta'] or jobs['chef'] = person['Thelma']) and
    jobs['chef'] != jobs['clerk']
;

#  Roberta is not a boxer.
s.t. c4: person['Roberta'] != jobs['boxer'];

#  Pete has no education past the ninth grade.
s.t. c5: 
   person['Pete'] != jobs['teacher'] and 
   person['Pete'] != jobs['police_officer'] and 
   person['Pete'] != jobs['nurse']
;

# Roberta, [and] the chef, and the police officer went golfing together.
s.t. c6:
    person['Roberta'] != jobs['chef'] and
    jobs['chef']    != jobs['police_officer'] and
    person['Roberta'] != jobs['police_officer']
;

# From the name of the job
s.t. c7: 
   (jobs['actor'] = person['Steve'] or 
    jobs['actor'] = person['Pete'])
;


data;

param n := 4;

set Persons = Roberta Thelma Steve Pete;
set Jobs = chef guard nurse clerk police_officer teacher actor boxer;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display person, jobs;

for{i in 1..n} {
   for{j in Persons} { if person[j] = i then printf "%-7s: ", j };
   for{j in Jobs} { if jobs[j] = i then printf "%-7s ", j };
   printf "\n";
}
