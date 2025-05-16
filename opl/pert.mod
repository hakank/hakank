/* 

  Simple PERT model in OPL.

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int maxTime = 30;
int n = 11;
int numDependencies = 15;
                                    //  a  b  c  d  e  f  g  h  j  k  Send 
int Times[1..n] = [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1]; // time per activity
int Dependencies[1..numDependencies, 1..2] =
[
  [2,1],  // Sb >= Sa + 7
  [4,1],  // Sd >= Sa + 7
  [3,2],  // Sc >= Sb + 3
  [5,3],  // Se >= Sc + 1
  [5,4],  // Se >= Sd + 8
  [7,3],  // Sg >= Sc + 1
  [7,4],  // Sg >= Sd + 8
  [6,4],  // Sf >= Sd + 8
  [6,3],  // Sf >= Sc + 1
  [8,6],  // Sh >= Sf + 1
  [9,8],  // Sj >= Sh + 3
  [10,7], // Sk >= Sg + 1
  [10,5], // Sk >= Se + 1
  [10,9], // Sk >= Sj + 2
  [11,10] // Send >= Sk + 1
];


// decision variables
dvar int Start[1..n] in 0..maxTime; // when the activity start
dvar int SumTimes; // = sum(i in 1..n) Start[i];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(Start,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

minimize Start[n];
constraints {

   SumTimes == sum(i in 1..n) Start[i];

   forall(i in 1..numDependencies) {
      Start[Dependencies[i,1]] >= (Start[Dependencies[i,2]] + Times[Dependencies[i,2]]);
   }


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("Start:", t.Start);
        writeln("SumTimes: ", t.SumTimes);
        writeln("Start[n]: ", t.Start[t.n]);
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
