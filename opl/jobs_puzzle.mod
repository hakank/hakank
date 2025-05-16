/* 

  Jobs Puzzle in OPL.

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


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
range p = 1..n;

int Roberta = 1;
int Thelma = 2;
int Steve = 3;
int Pete = 4;
int Persons[1..4] = [Roberta, Thelma, Steve, Pete];
string Persons_s[1..4] = ["Roberta", "Thelma", "Steve", "Pete"];

// decision variables
range job = 1..4;
dvar int chef in job;
dvar int guard in job;
dvar int nurse in job;
dvar int clerk in job;
dvar int police_officer in job;
dvar int teacher in job;
dvar int actor in job;
dvar int boxer in job;

dvar int Jobs[1..8] = [chef, guard, nurse, clerk, police_officer, teacher, actor, boxer];
string Jobs_s[1..8] = ["Chef", "Guard", "Nurse", "Clerk", "Police", "Teacher", "Actor", "Boxer"];


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(Jobs,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );


  cp.setSearchPhases(s);

 
}


constraints {

    //  Each holds exactly two jobs.
    forall(i in 1..n) {
       count(Jobs, i) == 2;
    }

    //  The job of nurse is held by a male.
    nurse == Steve || nurse == Pete;

    //  The husband of the chef is the clerk.
    clerk == Steve || clerk == Pete;
    chef == Roberta || chef == Thelma;
    chef != clerk;

    //  Roberta is not a boxer.
    Roberta != boxer;

    //  Pete has no education past the ninth grade.
    Pete != teacher;
    Pete != police_officer;
    Pete != nurse;

    // Roberta, [and] the chef, and the police officer went golfing together.
    Roberta != chef;
    chef    != police_officer;
    Roberta != police_officer;

     // From the name of the job
    (actor == Steve || actor == Pete);


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("Jobs:", t.Jobs);
        for(i = 1; i <= 8; i++) {
           writeln(t.Jobs_s[i], ": ", t.Persons_s[t.Jobs[i]]);
        }
        writeln();
        sol++;
     }
     cp.endSearch();

     writeln();
     var info = cp.info;
     writeln("#ModelVariables: ", info.NumberOfModelVariables);
     writeln("#Variables: ", info.NumberOfVariables);
     writeln("#Constraints: ", info.NumberOfConstraints);
     writeln("#Choice points: ", info.NumberOfChoicePoints);
     writeln("#Fails: ", info.NumberOfFails);
     writeln("#Branches: ", info.NumberOfBranches);
     writeln("TotalTime: ", info.TotalTime);
     writeln("SolveTime: ", info.SolveTime);
     writeln();

     writeln("#solutions: ", sol);

} 
