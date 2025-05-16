/* 

  Crew allocation in OPL.

  From Gecode example crew
  examples/crew.cc
  (Original text from crew.cc)
  """
  * Example: Airline crew allocation
  *
  * Assign 20 flight attendants to 10 flights. Each flight needs a certain
  * number of cabin crew, and they have to speak certain languages.
  * Every cabin crew member has two flights off after an attended flight.
  *
  """

  Note: This model finds the minimum (19) very fast, but then it takes
        "forever" to prove that it's the optimal value.
        The original statement is - what I know - not an optimization
        problem.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int numPersons = 20; // number of persons
int attributes[1..numPersons, 1..5] = 
// steward, hostess, french, spanish, german
[
  [1,0,0,0,1],   // Tom     = 1
  [1,0,0,0,0],   // David   = 2
  [1,0,0,0,1],   // Jeremy  = 3
  [1,0,0,0,0],   // Ron     = 4
  [1,0,0,1,0],   // Joe     = 5
  [1,0,1,1,0],   // Bill    = 6
  [1,0,0,1,0],   // Fred    = 7
  [1,0,0,0,0],   // Bob     = 8
  [1,0,0,1,1],   // Mario   = 9
  [1,0,0,0,0],   // Ed      = 10
  [0,1,0,0,0],   // Carol   = 11
  [0,1,0,0,0],   // Janet   = 12
  [0,1,0,0,0],   // Tracy   = 13
  [0,1,0,1,1],   // Marilyn = 14
  [0,1,0,0,0],   // Carolyn = 15
  [0,1,0,0,0],   // Cathy   = 16
  [0,1,1,1,1],   // Inez    = 17
  [0,1,1,0,0],   // Jean    = 18
  [0,1,0,1,1],   // Heather = 19
  [0,1,1,0,0]    // Juliet  = 20
];

int numFlights = 10;                           // number of flights

// required crew per flight 
int requiredCrew[1..numFlights,1..6] = 
[  
   [4,1,1,1,1,1], // Flight 1
   [5,1,1,1,1,1], // Flight 2
   [5,1,1,1,1,1], // ..
   [6,2,2,1,1,1],
   [7,3,3,1,1,1],
   [4,1,1,1,1,1],
   [5,1,1,1,1,1],
   [6,1,1,1,1,1],
   [6,2,2,1,1,1],
   [7,3,3,1,1,1]  // Flight 10
];



// decision variables
dvar int crew[1..numFlights, 1..numPersons] in 0..1; // the crew schedule
// objective to (perhaps) minimize : number of persons working
dvar int z in 1..numPersons;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(crew,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


minimize z;
constraints {

  z == sum(p in 1..numPersons) (sum(f in 1..numFlights) (crew[f,p]) > 0);

  forall(f in 1..numFlights) {
     // size of crew
     sum(i in 1..numPersons) (crew[f,i]) == requiredCrew[f, 1];

     // attribute and requirements
     forall(j in 1..5) {
       sum(i in 1..numPersons) (attributes[i,j]*crew[f,i]) >=  requiredCrew[f,j+1];
     }
  }

  // after a flight, break for two flights
  forall(f in 1..numFlights-2, i in 1..numPersons) {
     (crew[f,i] + crew[f+1,i] + crew[f+2,i]) <= 1;
  }

  //// extra contraint: all persons must work at least two times
  //// (then the minimum is - of course - 20)
  // forall(i in 1..numPersons) {
  //   sum(f in 1..numFlights) (crew[f,i]) >= 2;
  // }


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("crew  :", t.crew);
        writeln("z: ", t.z);
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
