/*

  Jobs puzzle in SICStus Prolog.

  (This is a standard problem in Automatic Reasoning.)
 
 
  From http://www-unix.mcs.anl.gov/~wos/mathproblems/jobs.html
  """
  Jobs Puzzle
  
  There are four people:  Roberta, Thelma, Steve, and Pete.
   Among them, they hold eight different jobs.
   Each holds exactly two jobs.
   The jobs are chef, guard, nurse, clerk, police officer (gender 
   not implied), teacher, actor, and boxer.
   The job of nurse is held by a male.
   The husband of the chef is the clerk.
   Roberta is not a boxer.
   Pete has no education past the ninth grade.
   Roberta, the chef, and the police officer went golfing together.
 
   Question:  Who holds which jobs?
 
 
  The answer:
  Chef       Thelma
  Guard      Roberta
  Nurse      Steve
  Clerk      Pete
  Police     Steve
  Teacher    Roberta
  Actor      Pete
  Boxer      Thelma

  
  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/jobs_puzzle.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        Roberta = 1,
        Thelma = 2,
        Steve = 3,
        Pete = 4,
        
        Persons = [Roberta, Thelma, Steve, Pete],
        
        Jobs = [Chef, _Guard, Nurse, Clerk, PoliceOfficer, Teacher, Actor, Boxer],
        domain(Jobs,1,4),
        
        
        %  Each holds exactly two jobs.
        global_cardinality(Jobs,[1-2,2-2,3-2,4-2]),

        %  The job of nurse is held by a male.
        (Nurse #= Steve #\/ Nurse #= Pete),

        %  The husband of the chef is the clerk.
        (Clerk #= Steve #\/ Clerk #= Pete),
        (Chef #= Roberta #\/ Chef #= Thelma),
        Chef #\= Clerk,

        %  Roberta is not a boxer.
        Roberta #\= Boxer ,

        %  Pete has no education past the ninth grade.
        Pete #\= Teacher , Pete #\= PoliceOfficer, Pete #\= Nurse,

        % Roberta, [and] the chef, and the police officer 
        % went golfing together.
        Roberta #\= Chef , 
        Chef    #\= PoliceOfficer ,
        Roberta #\= PoliceOfficer ,

        % From the name of the job
        (Actor #= Steve #\/ Actor #= Pete),

        % search
        labeling([],Jobs),

        % output
        write(Jobs),nl,
        PersonsStr = ['Roberta', 'Thelma', 'Steve', 'Pete'],
        JobsStr = ['Chef', 'Guard', 'Nurse', 'Clerk', 'Police',
                   'Teacher', 'Actor', 'Boxer'],
        ( foreach(J,Jobs),
          foreach(JS,JobsStr),
          param(PersonsStr,Persons) do
              ( foreach(P,Persons),
                foreach(PS,PersonsStr),
                param(J,JS) do
                    P == J ->
                    format('~w\t~w',[JS,PS])
              ;
                    true
              ),
              nl
        ),
        
        fd_statistics.


