/* 

  Map coloring in OPL.

  Simple map coloring problem.
  Cf. map1.mod
  Cf. map2.mod

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range colors = 1..4;

// decision variables
dvar int belgium in colors;
dvar int denmark in colors;
dvar int france in colors;
dvar int germany in colors;
dvar int netherlands in colors;
dvar int luxembourg in colors;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(color,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);
  */
}


constraints {

    belgium != france; 
    belgium != germany; 
    belgium != netherlands;
    belgium != luxembourg;
    denmark != germany; 
    france != germany; 
    france != luxembourg; 
    germany != luxembourg;
    germany != netherlands;    

 
    // symmetry breaking: belgium has color=1
    belgium == 1;
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("belgium: ", thisOplModel.belgium);
      writeln("denmark: ", thisOplModel.denmark);
      writeln("france: ", thisOplModel.france);
      writeln("germany: ", thisOplModel.germany);
      writeln("netherlands: ", thisOplModel.netherlands);
      writeln("luxembourg: ", thisOplModel.luxembourg);
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
