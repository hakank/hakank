/* 

  Global constraint global_contiguity  in OPL.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
  In addition, all variables assigned to value 1 appear contiguously.
  
  Example:
  (<0,​1,​1,​0>)
  
  The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
  no more than one group of contiguous 1.
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;

// decision variables
dvar int x[1..n] in 0..1;
dvar int y[1..n] in 0..2; // extra variable

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

constraints {

    forall(i in 2..n) {
       y[i-1] <= y[i];
    }

    forall(i in 1..n) {
        (x[i] == 1) == (y[i] == 1);
    }

    (sum(i in 1..n) x[i] == 0) == (sum(i in 1..n) y[i] == 0);

    // make it a little more interesting: there must be atleast three 1s.
    // count(x, 1) == 3;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("y:", t.y);
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
