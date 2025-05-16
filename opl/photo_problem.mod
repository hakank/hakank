/* 

  Photo problem in OPL.

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for taking 
  a photo. Some of them have preferences next to whom they want to stand:

     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.

  Obviously, it is impossible to satisfy all preferences. Can you find an alignment 
  that maximizes the number of satisfied preferences?
  """

  Oz solution: 
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]
  
  There are 8 solutions:
    positions = [3, 1, 6, 5, 2, 4, 7]
    positions = [3, 1, 7, 5, 2, 4, 6]
    positions = [3, 2, 6, 5, 1, 4, 7]
    positions = [3, 2, 7, 5, 1, 4, 6]
    positions = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
    positions = [5, 6, 2, 3, 7, 4, 1]
    positions = [5, 7, 1, 3, 6, 4, 2]
    positions = [5, 7, 2, 3, 6, 4, 1]


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = ...;
int preferences[1..n, 1..n] = ...;

/*
int n = 7;
int preferences[1..n, 1..n] =
[
//  B C D F G M P
   [0,0,0,0,1,1,0],  // Betty  1
   [1,0,0,0,1,0,0],  // Chris  2
   [0,0,0,0,0,0,0],  // Donald 3
   [0,0,1,0,0,1,0],  // Fred   4
   [0,0,0,0,0,0,0],  // Gary   5
   [0,0,0,0,0,0,0],  // Mary   6
   [0,0,1,1,0,0,0]  // Paul   7
];
*/

// decision variables
dvar int x[1..n] in 1..n; 
dvar int z; // number of fullfilled preferences


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

maximize z;
constraints {

   allDifferent(x);

   z == sum(i,j in 1..n: preferences[i,j] == 1) 
                   (abs(x[i]-x[j]) == 1);

   // for all solutions (first problem)
   // z == 6;

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
