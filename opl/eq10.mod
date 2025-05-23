/* 

  Eq10 in OPL.

  Standard benchmark problem.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range r = 0..n;

// decision variables
dvar int X1 in r;
dvar int X2 in r;
dvar int X3 in r;
dvar int X4 in r;
dvar int X5 in r;
dvar int X6 in r;
dvar int X7 in r;

dvar int x[1..7] = [X1,X2,X3,X4,X5,X6,X7];


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

	   0+98527*X1+34588*X2+5872*X3+59422*X5+65159*X7 
	== 1547604+30704*X4+29649*X6;

	   0+98957*X2+83634*X3+69966*X4+62038*X5+37164*X6+85413*X7 
	== 1823553+93989*X1;

	   900032+10949*X1+77761*X2+67052*X5 
	== 0+80197*X3+61944*X4+92964*X6+44550*X7;

	   0+73947*X1+84391*X3+81310*X5 
	== 1164380+96253*X2+44247*X4+70582*X6+33054*X7;

	   0+13057*X3+42253*X4+77527*X5+96552*X7 
	== 1185471+60152*X1+21103*X2+97932*X6;

	   1394152+66920*X1+55679*X4 
	== 0+64234*X2+65337*X3+45581*X5+67707*X6+98038*X7;

	   0+68550*X1+27886*X2+31716*X3+73597*X4+38835*X7 
	== 279091+88963*X5+76391*X6;

	   0+76132*X2+71860*X3+22770*X4+68211*X5+78587*X6 
	== 480923+48224*X1+82817*X7;

	   519878+94198*X2+87234*X3+37498*X4 
	== 0+71583*X1+25728*X5+25495*X6+70023*X7;

	   361921+78693*X1+38592*X5+38478*X6 
	== 0+94129*X2+43188*X3+82528*X4+69025*X7;

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
