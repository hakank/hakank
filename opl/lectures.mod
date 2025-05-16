/* 

  Lectures problem in OPL.

  Biggs: Discrete Mathematics (2nd ed), page 187.
  """
  Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
  Among the the potential audience there are people who wish to hear both

   - v1 and v2
   - v1 and v4
   - v3 and v5
   - v2 and v6
   - v4 and v5
   - v5 and v6
   - v1 and v6

  How many hours are necessary in order that the lectures can be given
  without clashes?
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6; // number of nodes
int edges = 7; // number of edges
range r = 1..n; // nodes

//
// The schedule requirements:
//    lecture a cannot be held at the same time as b
//
int g[1..edges, 1..2] = 
  [
   [1, 2],
   [1, 4],
   [3, 5],
   [2, 6],
   [4, 5],
   [5, 6],
   [1, 6]
  ];


// decision variables
dvar int v[r] in r;
dvar int max_c; // number of times (colors)


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(v,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


minimize max_c;
constraints {

   // maximum(max_c, v)
   forall(i in r) {
     max_c >= v[i];
   }

   forall(i in 1..edges) {
      v[g[i,1]] != v[g[i,2]];
   }

   // symmetry breaking: v1 has the color 1, v2 has either color 1 or 2
   // (this should be general enough for a general model)
   v[1] == 1;
   v[2] <= 2;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("v    :", t.v);
        writeln("max_c: ", t.max_c);
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
