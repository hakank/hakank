/* 

  Set covering in OPL.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.

  Solution: Sets 3,6,7. Total elements choosen: 15
  Another solution with three sets is {4,6,7}, but the total choosen elements is 17.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int num_sets = 7;
int num_elements = 12;
int belongs[1..num_sets, 1..num_elements] = 
[
   //  1 2 3 4 5 6 7 8 9 0 1 2  elements
      [1,1,0,0,0,0,0,0,0,0,0,0],      // Set 1
      [0,1,0,0,0,0,0,1,0,0,0,0],      //     2
      [0,0,0,0,1,1,0,0,0,0,0,0],      //     3
      [0,0,0,0,0,1,1,0,0,1,1,0],      //     4
      [0,0,0,0,0,0,0,0,1,1,0,0],      //     5
      [1,1,1,0,1,0,0,0,1,1,1,0],      //     6
      [0,0,1,1,0,0,1,1,0,0,1,1]       //     7
];


// decision variables
dvar int x[1..num_sets] in 0..1;
dvar int z;
dvar int total_elements;


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


minimize z;
// minimize total_elements;
// minimize 10*z+total_elements;
constraints {

  total_elements == sum(i in 1..num_sets, j in 1..num_elements) belongs[i,j]*x[i];
  z == sum(i in 1..num_sets) x[i];

  forall(j in 1..num_elements) {
       sum(i in 1..num_sets) x[i]*belongs[i,j] >= 1;
  }
   
}


main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("z: ", t.z);
        writeln("total_elements: ", t.total_elements);
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
