/* 

  Set partition, set covering in OPL.

  Example from Lundgren, Ronnqvist, Varbrand 
  "Optimeringslara" [Optimization theory], page 408.
  
  We want to minimize the cost of the alternatives which covers all the 
  objects, i.e. all objects must be choosen. The requirement is than an object 
  may be selected _exactly_ once.

  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7

  The problem has a unique solution of z = 49 where alternatives 
  3, 5, and 9 is selected. 

  If we, however, allow that an object is selected more than one time, 
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives 4, 8, and 10 is selected, where object 5 is 
  selected twice (alt. 4 and 8). It's an unique solution as well.
  

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int set_partition = 0;
int num_alternatives =  10;
int costs[1..num_alternatives] = [ 19, 16, 18, 13, 15, 19, 15, 17, 16, 15];
int num_objects = 8;

// the alternatives and the objects they contain
int a[1..num_alternatives, 1..num_objects] = 
[
// 1 2 3 4 5 6 7 8  the objects 
   [1,0,0,0,0,1,0,0],  // alternative 1
   [0,1,0,0,0,1,0,1],  // alternative 2
   [1,0,0,1,0,0,1,0],  // alternative 3
   [0,1,1,0,1,0,0,0],  // alternative 4
   [0,1,0,0,1,0,0,0],  // alternative 5
   [0,1,1,0,0,0,0,0],  // alternative 6
   [0,1,1,1,0,0,0,0],  // alternative 7
   [0,0,0,1,1,0,0,1],  // alternative 8
   [0,0,1,0,0,1,0,1],  // alternative 9
   [1,0,0,0,0,1,1,0]   // alternative 10
];


// decision variables
dvar int x[1..num_alternatives] in 0..1;
dvar int z;


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
constraints {

  z == sum(i in 1..num_alternatives) x[i]*costs[i];

  forall(j in 1..num_objects) {
     // all objects must be covered _exactly_ once: set partition
     if (set_partition == 1) {
        sum(i in 1..num_alternatives) x[i]*a[i,j] == 1;
     } else {
       // variant: all objects must be covered _at least_ once: set covering
       sum(i in 1..num_alternatives) x[i]*a[i,j] >= 1;
    }
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
