/* 

  Pigeon hole problem in OPL.

  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1 pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon, its hole number. Obviously, there is a solution iff n <= m.
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 3;  // n pigeons
int m = 10; // m pigeon holes

// decision variables
dvar int x[1..n, 1..m] in 0..1;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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


constraints {

   // max 1 pigeon per pigeon hole
   forall(j in 1..m) {
     sum(i in 1..n) x[i,j] <= 1;
   }

   // all pigeon must be placed at exactly one hole
   forall(i in 1..n) {
     sum(j in 1..m) x[i,j] == 1;
   }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:     ", t.x);
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
