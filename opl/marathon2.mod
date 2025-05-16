/* 

  Marathon puzzle in OPL.

  From Xpress example
  http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
  """
  Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
  have arrived as the first six at the Paris marathon.
  Reconstruct their arrival order from the following
  information:
  a) Olivier has not arrived last
  b) Dominique, Pascal and Ignace have arrived before Naren
     and Olivier
  c) Dominique who was third last year has improved this year.
  d) Philippe is among the first four.
  e) Ignace has arrived neither in second nor third position.
  f) Pascal has beaten Naren by three positions.
  g) Neither Ignace nor Dominique are on the fourth position.
  
     (c) 2002 Dash Associates
    author: S. Heipcke, Mar. 2002
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;

// decision variables
dvar int Dominique in 1..n;
dvar int Ignace in 1..n;
dvar int Naren in 1..n;
dvar int Olivier in 1..n;
dvar int Philippe in 1..n;
dvar int Pascal in 1..n;

dvar int runners[1..n] = [Dominique, Ignace, Naren, Olivier, Philippe, Pascal];
dvar int runners_inv[1..n] in 1..n; // for presentation

string runners_s[1..n] = ["Dominique", "Ignace", "Naren", "Olivier", "Philippe", "Pascal"];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(runners,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

  allDifferent(runners);
  inverse(runners, runners_inv);
  
  // a: Olivier not last
  Olivier    != n;

  // b: Dominique, Pascal and Ignace before Naren and Olivier
  Dominique  < Naren;
  Dominique  < Olivier;
  Pascal     < Naren;
  Pascal     < Olivier;
  Ignace     < Naren;
  Ignace     < Olivier;

  // c: Dominique better than third
  Dominique  < 3; 

  // d: Philippe is among the first four
  Philippe   <= 4 ;

  // e: Ignace neither second nor third
  Ignace     != 2; 
  Ignace     != 3; 

  // f: Pascal three places earlier than Naren
  Pascal + 3 == Naren; 

  // g: Neither Ignace nor Dominique on fourth position
  Ignace     != 4;
  Dominique  != 4;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("\nrunners:", t.runners);
        for(i = 1; i <= t.n; i++) {
          writeln(t.runners_s[i], " place: ", t.runners_inv[i]);
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
