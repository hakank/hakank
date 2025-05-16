/* 

  Traffic lights problem in OPL.

  CSPLib problem 16
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  """
  Specification:
  Consider a four way traffic junction with eight traffic lights. Four of 
  the traffic lights are for the vehicles and can be represented by the 
  variables V1 to V4 with domains 
  {r,ry,g,y} (for red, red-yellow, green and yellow). 
   The other four traffic lights are for the pedestrians and can be 
   represented by the variables P1 to P4 with domains {r,g}.
  
  The constraints on these variables can be modelled by quaternary 
  constraints on 
  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples 
  {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.

  It would be interesting to consider other types of junction (e.g. five roads 
  intersecting) as well as modelling the evolution over time of the 
  traffic light sequence. 
  ...

  Results
  Only 2^2 out of the 2^12 possible assignments are solutions.
  
  (V1,P1,V2,P2,V3,P3,V4,P4) = 
     {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
     [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}


  The problem has relative few constraints, but each is very tight. 
  Local propagation appears to be rather ineffective on this problem.    
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4; 
int r  = 1; // red
int ry = 2; // red-yellow
int g  = 3; // green
int y  = 4; // yellow

range Cars = r..y;

tuple A {
  int a;
  int b;
  int c;
  int d;
}

{A} allowed= 
{
 <r,r,g,g>, 
 <ry,r,y,r>, 
 <g,g,r,r>, 
 <y,r,ry,r>
};

// decision variables
dvar int V[1..n] in Cars;
dvar int P[1..n] in Cars; // in {r,g}; 


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(V,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

   forall(i in 1..n) {
      P[i] == r || P[i] == g;
   }

   forall(i,j in 1..n: j == (1+i) % 4)  {
     allowedAssignments(allowed, V[i], P[i], V[j], P[j]);
   }
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var V = thisOplModel.V;
      var P = thisOplModel.P;
      writeln("V: ", V);
      writeln("P: ", P);
      for(i = 1; i <= thisOplModel.n; i++) {
        write(V[i], " ", P[i], "  ");
      }
      writeln();
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
