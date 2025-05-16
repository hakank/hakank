/* 

  Eq20 in OPL.

  Standard benchmark problem.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range r = 0..n;

// decision variables
dvar int X0 in r;
dvar int X1 in r;
dvar int X2 in r;
dvar int X3 in r;
dvar int X4 in r;
dvar int X5 in r;
dvar int X6 in r;

dvar int x[0..6] = [X0,X1,X2,X3,X4,X5,X6];


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


constraints {

        -76706*X0 + 98205*X1 + 23445*X2 + 67921*X3 + 24111*X4 + 
        -48614*X5 + -41906*X6
        == 821228;
        87059*X0 + -29101*X1 + -5513*X2 + -21219*X3 + 22128*X4 +
        7276*X5 + 57308*X6
        == 22167;
        -60113*X0 + 29475*X1 + 34421*X2 + -76870*X3 + 62646*X4 + 
        29278*X5 + -15212*X6
        == 251591;
        49149*X0 + 52871*X1 + -7132*X2 + 56728*X3 + -33576*X4 + 
        -49530*X5 + -62089*X6
        == 146074;
        -10343*X0 + 87758*X1 + -11782*X2 + 19346*X3 + 70072*X4 + 
        -36991*X5 + 44529*X6
        == 740061;
        85176*X0 + -95332*X1 + -1268*X2 + 57898*X3 + 15883*X4 +
        50547*X5 + 83287*X6
        == 373854;
        -85698*X0 + 29958*X1 + 57308*X2 + 48789*X3 + -78219*X4 +
        4657*X5 + 34539*X6
        == 249912;
        -67456*X0 + 84750*X1 + -51553*X2 + 21239*X3 + 81675*X4 + 
        -99395*X5 + -4254*X6
        == 277271;
        94016*X0 + -82071*X1 + 35961*X2 + 66597*X3 + -30705*X4 + 
        -44404*X5 + -38304*X6
        == 25334;
        -60301*X0 + 31227*X1 + 93951*X2 + 73889*X3 + 81526*X4 + 
        -72702*X5 + 68026*X6
        == 1410723;
        -16835*X0 + 47385*X1 + 97715*X2 + -12640*X3 + 69028*X4 + 
        76212*X5 + -81102*X6
        == 1244857;
        -43277*X0 + 43525*X1 + 92298*X2 + 58630*X3 + 92590*X4 +
        -9372*X5 + -60227*X6
        == 1503588;
        -64919*X0 + 80460*X1 + 90840*X2 + -59624*X3 + -75542*X4 + 
        25145*X5 + -47935*X6
        == 18465;
        -45086*X0 + 51830*X1 + -4578*X2 + 96120*X3 + 21231*X4 +
        97919*X5 + 65651*X6
        == 1198280;
        85268*X0 + 54180*X1 + -18810*X2 + -48219*X3 + 6013*X4 +
        78169*X5 + -79785*X6
        == 90614;
        8874*X0 + -58412*X1 + 73947*X2 + 17147*X3 + 62335*X4 +
        16005*X5 + 8632*X6
        == 752447;
        71202*X0 + -11119*X1 + 73017*X2 + -38875*X3 + -14413*X4 + 
        -29234*X5 + 72370*X6
        == 129768;
        1671*X0 + -34121*X1 + 10763*X2 + 80609*X3 + 42532*X4 +
        93520*X5 + -33488*X6
        == 915683;
        51637*X0 + 67761*X1 + 95951*X2 + 3834*X3 + -96722*X4 +
        59190*X5 + 15280*X6
        == 533909;
        -16105*X0 + 62397*X1 + -6704*X2 + 43340*X3 + 95100*X4 + 
        -68610*X5 + 58301*X6
        == 876370;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x: ", t.x);
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
