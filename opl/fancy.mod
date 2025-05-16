/* 

  Mr Greenguest puzzle (fancy dress) in OPL.

  Problem (and LPL) code in
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2

  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range bool = 0..1;

// decision variables
dvar int t in bool;
dvar int h in bool;
dvar int r in bool;
dvar int s in bool;
dvar int n in bool;
dvar int cost;

execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);
  */
 
}

minimize cost;
constraints {

  // This is a straight translation from the LPL code
  (t==1=>r==1) || n==1;
  ((s==1 || r==1) => (t==1 || h==1)) || n==1;
  ((r==1 || h==1 || s==0) => t==1) || n==1;

  cost == 10*t + 2*h + 12*s + 11*n;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("t:", t.t);
        writeln("h:", t.h);
        writeln("r:", t.r);
        writeln("s:", t.s);
        writeln("n:", t.n);
        writeln("cost:", t.cost);
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
