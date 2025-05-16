/* 

  Post office problem in OPL.

  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/
  """
  From "Operations Research":

  A post office requires a different number of full-time employees working 
  on different days of the week [summarized below]. Union rules state that 
  each full-time employee must work for 5 consecutive days and then receive 
  two days off. For example, an employee who works on Monday to Friday must 
  be off on Saturday and Sunday. The post office wants to meet its daily 
  requirements using only full-time employees. Minimize the number of 
  employees that must be hired.
  """
  To summarize the important information about the problem:

    * Every full-time worker works for 5 consecutive days and takes 2 days off
    * Day 1 (Monday): 17 workers needed
    * Day 2 : 13 workers needed
    * Day 3 : 15 workers needed
    * Day 4 : 19 workers needed
    * Day 5 : 14 workers needed
    * Day 6 : 16 workers needed
    * Day 7 (Sunday) : 11 workers needed

  The post office needs to minimize the number of employees it needs to hire to meet its demand. 


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range DAYS = 0..6;
int Need[DAYS] = [17, 13, 15, 19, 14, 16, 11];

// Total cost for the 5 day schedule.
// Base cost per day is 100.
// Working saturday is 100 extra
// Working sunday is 200 extra.
int Cost[DAYS] = [500, 600, 800, 800, 800, 800, 700];



// decision variables
dvar int x[DAYS] in 0..6;
dvar int z; // total cost
dvar int num_workers; 


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
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


minimize z;
constraints {
   z == sum(i in DAYS) x[i] * Cost[i]; 
   num_workers == sum(i in DAYS) x[i];
   forall(i in DAYS) {
     sum(j in DAYS: j != (i+5) % 7 && j != (i+6) % 7) x[j] >= Need[i];
   }
}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x :", t.x);
        writeln("z :", t.z);
        writeln("num_workers :", t.num_workers);
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
