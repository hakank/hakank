/* 

  Some explorations of ISBN13 in OPL.

  See http://en.wikipedia.org/wiki/ISBN

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 13;

// decision variables
dvar int isbn[1..n] in 0..9;
dvar int mult0 in 1..9;
dvar int mult1 in 1..9;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(isbn,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);


}



constraints {

   // isbn == [9,7,8,1,5,5,8,6,0,8,9,0,0];
   isbn[1] == 9;
   isbn[2] == 7;
   isbn[3] == 8;
   isbn[4] == 1;
   isbn[5] == 5;
   isbn[6] == 5;
   isbn[7] == 8;
   isbn[8] == 6;
   isbn[9] == 0;
   isbn[10] == 8;
   isbn[11] == 9;
   isbn[12] == 0;
   // isbn[13] == 0;

   isbn[n] == (10 - sum(i in 1..n-1) (
                     (i % 2 == 0)*(isbn[i] * mult0) // 3
                     +
                     (i % 2 == 1)*(isbn[i] * mult1) // 1
                   ) % 10) % 10;

   // The correct values when calculating the check sum
   // mult0 == 3;
   mult1 == 1;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("isbn :", t.isbn);
        writeln("mult0 :", t.mult0);
        writeln("mult1 :", t.mult1);
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
