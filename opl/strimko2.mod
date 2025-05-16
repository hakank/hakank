/* 

  Strimko puzzle in OPL.

  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """

  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm
  


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = ...;
int streams[1..n, 1..n] = ...;
int num_placed = ...;
int placed[1..num_placed, 1..3] = ...; 

/*
// Strimko Set 068
int n = 4;
// represent the different streams with integer 1..n
int streams[1..n, 1..n] = 
  [
    [1,2,2,4],
    [2,1,4,2],
    [3,4,1,3],
    [4,3,3,1]
  ];

int num_placed = 3;
int placed[1..num_placed, 1..3] = 
  [
    [2,2,3],
    [2,3,2],
    [3,3,1]
  ];
*/


// decision variables
dvar int x[1..n, 1..n] in 1..n;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
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
   // Latin Square
   forall(i in 1..n) {
       allDifferent(all(j in 1..n) x[i,j]);
       allDifferent(all(j in 1..n) x[j,i]);
   }

   // Streams
   forall(s in 1..n) {
       allDifferent(all(i,j in 1..n: streams[i,j] == s) x[i,j]);
   }


   // Placed
   forall(i in 1..num_placed) {
       x[placed[i,1], placed[i,2]] == placed[i,3];
   }


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      // writeln("x: ", t.x);
      for(i = 1; i <= t.n; i++) {
         for(j = 1; j <= t.n; j++) {
            write(t.x[i][j], " ");
         }
         writeln();
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
