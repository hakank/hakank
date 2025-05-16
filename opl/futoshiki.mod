/* 

  Futoshiki puzzle in OPL.

  http://en.wikipedia.org/wiki/Futoshiki
  http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley

  Model based on the Minion/Tailor example futoshiki.eprime .


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int  SIZE = 5;
// specify last index in array lt; lt[0] is first entry
int lastdx = 10;
range NUMQD = 0..lastdx;
range RANGE = 1..SIZE;
range VALUES = 0..SIZE;


// Example from Tailor model futoshiki.param/futoshiki.param
//
// Solution:
// 5 1 3 2 4
// 1 4 2 5 3
// 2 3 1 4 5
// 3 5 4 1 2
// 4 2 5 3 1
// 
// Futoshiki instance, by Andras Salamon
// specify the numbers in the grid
int values[RANGE, RANGE] = 
[
   [0, 0, 3, 2, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0],
   [0, 0, 0, 0, 0]
];

// list of < relations in the problem
// [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
int lt[NUMQD, 0..3] = 
[
   [1,2,1,1],
   [1,4,1,5],
   [2,3,1,3],
   [3,3,2,3],
   [3,4,2,4],
   [2,5,3,5],
   [3,2,4,2],
   [4,4,4,3],
   [5,2,5,1],
   [5,4,5,3],
   [5,5,4,5]
];


// decision variables
dvar int x[RANGE, RANGE] in RANGE;


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


constraints {

    // set initial values
    forall(row,col in RANGE: values[row,col] > 0) {
        x[row,col] == values[row,col];
    }

    // all rows have to be different
    forall(row in RANGE) {
        allDifferent(all(col in RANGE) x[row,col]);
    }

    // all columns have to be different
    forall(col in RANGE) {
        allDifferent(all(row in RANGE) x[row,col]);
    }

    // all < constraints are satisfied
    forall(i in NUMQD) {
       x[ lt[i,0], lt[i,1] ] < x[ lt[i,2], lt[i,3] ];
    }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     var alpha = " abcdefghijklmnopqrstuvwy";
     while (cp.next()) {
        var t = thisOplModel;
        for(i = 1; i <= t.SIZE; i++) {
            for(j = 1; j <= t.SIZE; j++) {
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
