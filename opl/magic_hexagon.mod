/* 

  Magic hexagon problem in OPL.

  Prob023: Magic Hexagon
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/prob023/prob023.pdf
  http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range N = 1..19;

// decision variables
dvar int  a in N;
dvar int  b in N;
dvar int  c in N;
dvar int  d in N;
dvar int  e in N;
dvar int  f in N;
dvar int  g in N;
dvar int  h in N;
dvar int  i in N;
dvar int  j in N;
dvar int  k in N;
dvar int  l in N;
dvar int  m in N;
dvar int  n in N;
dvar int  o in N;
dvar int  p in N;
dvar int  q in N;
dvar int  r in N;
dvar int  s in N;

dvar int LD[N] = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(LD,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}



constraints {

   allDifferent(LD);

   a + b + c ==  38;
   d + e + f + g ==  38;
   h + i + j + k + l ==  38; 
   m + n + o + p ==  38; 
   q + r + s ==  38; 
   a + d + h ==  38; 
   b + e + i + m ==  38; 
   c + f + j + n + q ==  38; 
   g + k + o + r ==  38; 
   l + p + s ==  38; 
   c + g + l ==  38; 
   b + f + k + p ==  38; 
   a + e + j + o + s ==  38; 
   d + i + n + r ==  38; 
   h + m + q ==  38; 

   a < c;
   a < h;
   a < l;
   a < q;
   a < s;
   c < h;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("LD :", t.LD);
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
