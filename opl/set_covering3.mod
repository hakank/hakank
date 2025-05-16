/* 

  Another set covering in OPL.

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int num_groups = 6;
int num_senators = 10;

int belongs[1..num_groups, 1..num_senators] = 
[
  [1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   // 1 southern
  [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   // 2 northern
  [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   // 3 liberals
  [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   // 4 conservative
  [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   // 5 democrats
  [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]    // 6 republicans

];


// decision variables
dvar int x[1..num_senators] in 0..1;
dvar int z;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

}


minimize z;
constraints {

  z == sum(i in 1..num_senators) x[i];

  forall(i in 1..num_groups) {
     sum(j in 1..num_senators) (x[j]*belongs[i,j]) >= 1;
  }
   
}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
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
