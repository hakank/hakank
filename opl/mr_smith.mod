/* 

  Mr Smith problem in OPL.

  From an IF Prolog example (http://www.ifcomputer.de/)
  """
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
      o If Mr Smith comes, his wife will come too.
      o At least one of their two sons Matt and John will come.
      o Either Mrs Smith or Tim will come, but not both.
      o Either Tim and John will come, or neither will come.
      o If Matt comes, then John and his father will
        also come.
   """

  The answer should be:
   Mr_Smith_comes      =  0
   Mrs_Smith_comes     =  0
   Matt_comes          =  0
   John_comes          =  1
   Tim_comes           =  1


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// decision variables
dvar int Mr_Smith in 0..1;
dvar int Mrs_Smith in 0..1;
dvar int Matt in 0..1;
dvar int John in 0..1;
dvar int Tim in 0..1;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);
  */


}


constraints {
   // If Mr Smith comes his wife will come too.
   Mr_Smith == 1 => Mrs_Smith == 1;

   // At least one of their two sons Matt and John will come.
   Matt == 1 || John == 1;

   // Either Mrs Smith or Tim will come but not both.
   // bool2int(Mrs_Smith) + bool2int(Tim) = 1 /\
   Mrs_Smith + Tim == 1;

   // Either Tim and John will come or neither will come.
   Tim == John;

   // If Matt comes /\ then John and his father will also come.
   Matt == 1 => (John == 1 && Mr_Smith == 1);
;
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("Mr_Smith : ", thisOplModel.Mr_Smith);
      writeln("Mrs_Smith: ", thisOplModel.Mrs_Smith);
      writeln("Matt     : ", thisOplModel.Matt);
      writeln("John     : ", thisOplModel.John);
      writeln("Tim      : ", thisOplModel.Tim);
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
