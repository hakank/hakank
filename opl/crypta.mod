/* 

  Cryptarithmetic puzzle in OPL.

  Prolog benchmark problem GNU Prolog (crypta.pl)
  """
  Name           : crypta.pl                                              
  Title          : crypt-arithmetic                                       
  Original Source: P. Van Hentenryck's book                               
  Adapted by     : Daniel Diaz - INRIA France                             
  Date           : September 1992                                         
                                                                         
  Solve the operation:                                                    
                                                                        
     B A I J J A J I I A H F C F E B B J E A                              
   + D H F G A B C D I D B I F F A G F E J E                              
   -----------------------------------------                              
   = G J E G A C D D H F A F J B F I H E E F                              
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range r = 0..n-1;


// decision variables
dvar int A in r;
dvar int B in r;
dvar int C in r;
dvar int D in r;
dvar int E in r;
dvar int F in r;
dvar int G in r;
dvar int H in r;
dvar int I in r;
dvar int J in r;
dvar int x[1..n] = [A,B,C,D,E,F,G,H,I,J];

dvar int Sr1 in 0..1;
dvar int Sr2 in 0..1;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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



constraints {

    allDifferent(x);

     A+10*E+100*J+1000*B+10000*B+100000*E+1000000*F+
     E+10*J+100*E+1000*F+10000*G+100000*A+1000000*F
     == F+10*E+100*E+1000*H+10000*I+100000*F+1000000*B+10000000*Sr1;

      
     C+10*F+100*H+1000*A+10000*I+100000*I+1000000*J+
     F+10*I+100*B+1000*D+10000*I+100000*D+1000000*C+Sr1
     == J+10*F+100*A+1000*F+10000*H+100000*D+1000000*D+10000000*Sr2;

     A+10*J+100*J+1000*I+10000*A+100000*B+
     B+10*A+100*G+1000*F+10000*H+100000*D+Sr2
     == C+10*A+100*G+1000*E+10000*J+100000*G;
 
}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x  :", t.x);
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
