/* 

  Grocery puzzle in OPL.

  From: Christian Schulte, Gert Smolka, Finite Domain
  "Constraint Programming in Oz. A Tutorial"
  http://www.mozart-oz.org/documentation/fdt/
  """
  A kid goes into a grocery store and buys four items. The cashier
  charges $7.11, the kid pays and is about to leave when the cashier
  calls the kid back, and says "Hold on, I multiplied the four items
  instead of adding them; I'll try again; Hah, with adding them the
  price still comes to $7.11''. What were the prices of the four items?
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;


// decision variables
dvar int x[1..4] in 0..711;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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

  x[1] + x[2] + x[3] + x[4] == 711;
  x[1] * x[2] * x[3] * x[4] == 711 * 100*100*100;
  
  // symmetry breaking
  x[1] < x[2];
  x[2] < x[3];
  x[3] < x[4];



}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x :", t.x);
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
