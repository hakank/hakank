/* 

  "Devil's Word" problem in OPL.

  I.e. addition/subtraction of an array of numbers to give a specific total (e.g. 666).

  Compare with my CGI program "Devil's Word"
     http://www.hakank.org/data_snooping/666.cgi


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

/* 
  My name ("Håkan Kjellerstrand") in ASCII numbers.
  Cf http://www.hakank.org/data_snooping/666.cgi?name=H//E5kan+Kjellerstrand&submit=ok
  which gives the solution:
  +72+229+107+97+110+32+75-106+101+108-108+101-114-115-116-114+97+110+100 = 666

  There are 288 different solutions...
*/
int n = 19;
int total = 666;
int arr[1..n] = [72, 229, 107, 97, 110, 32, 75, 106, 101, 108, 108, 101, 114, 115, 116, 114, 97, 110, 100];

int max_arr = max(i in 1..n) arr[i];

// decision variables
dvar int plus[1..n] in 0..1;  // is the number arr[i] to be added
dvar int minus[1..n] in 0..1; // or is it to be subtracted

// array with the number with correct sign
dvar int result[1..n] in -max_arr..max_arr;

// number of minus entries (perhaps to be minimized)
dvar int num_minus in 0..n;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(plus,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

// minimize num_minus;
constraints {

   num_minus == sum(i in 1..n) minus[i] == 1;

   // calculate the sum of the numbers in arr
   // total == sum(i in index_set(arr)) (arr[i]*plus[i] + (-arr[i])*minus[i])
   total == sum(i in 1..n) result[i];

   forall(i in 1..n) {
       // just one of plus and minus
       plus[i] + minus[i] == 1;
       // calculate the result array
       result[i] == arr[i]*plus[i] + (-arr[i])*minus[i];
   }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("total:", t.total);
        writeln("result:", t.result);
        writeln("plus:", t.plus);
        writeln("minus:", t.minus);
        writeln("num_minus:", t.num_minus);
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
