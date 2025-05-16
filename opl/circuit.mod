/* 

  Decomposition of global constraint circuit in OPL.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html
  """
  Enforce to cover a digraph G described by the NODES collection with one 
  circuit visiting once all vertices of G.
  
  Example
      (
      <
      index-1	succ-2,​
      index-2	succ-3,​
      index-3	succ-4,​
      index-4	succ-1
      >
     )
  The circuit constraint holds since its NODES argument depicts the 
  following Hamiltonian circuit visiting successively the vertices 
  1, 2, 3, 4 and 1.
  """

  This also generates the path of the circuit (the "p" array),
  where we assume that the path always start with 1.
  Note that the path is just the z array rotated by 1.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8;

// decision variables
dvar int x[1..n] in 1..n; // The successors
dvar int z[1..n] in 1..n; // Orbits of x[1]
dvar int p[1..n] in 1..n; // The path.

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";
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

      allDifferent(x);
      allDifferent(z);

      z[1] == x[1];

      //
      // put the orbit of x[1] in z[1..n]
      // z[i] must not be 1 until i = n and then must be 1.
      //
      forall(i in 2..n) {
          z[i] == x[z[i-1]];
      }


     // may not be 1 for i < n
     forall(i in 1..n-1) { z[i] != 1; }

     // when i = n it must be 1
     z[n] == 1;

     // Testing
     // x[2] == n;

     // The path
     p[1] == 1;
     forall(i in 2..n) {
       p[i] == x[p[i-1]];
     }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x : ", thisOplModel.x);
      // writeln("z : ", thisOplModel.z);
      writeln("p : ", thisOplModel.p);
      writeln();
      sol++;
      // just the first solutions for larger values of n
      if (thisOplModel.n > 8) {
        break;
      }
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
