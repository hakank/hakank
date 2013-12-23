/**
  *
  * Mr Smith problem in Choco3.
  *
  * From an IF Prolog example (http://www.ifcomputer.de/)
  * """
  * The Smith family and their three children want to pay a visit but they
  * do not all have the time to do so. Following are few hints who will go
  * and who will not:
  *  o If Mr Smith comes, his wife will come too.
  *  o At least one of their two sons Matt and John will come.
  *  o Either Mrs Smith or Tim will come, but not both.
  *  o Either Tim and John will come, or neither will come.
  *  o If Matt comes, then John and his father will
  *    also come.
  * """
  * 
  * The answer should be:
  * MrSmith      =  0
  * MrsSmith     =  0
  * Matt         =  0
  * John         =  1
  * Tim          =  1
  *
  *
  * This Choco3 model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my Choco page: http://www.hakank.org/choco/ 
  *
  */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class MrSmith extends AbstractProblem {

  int n = 5;

  String[] x_s = {"Mr Smith","Mrs Smith", "Matt", "John", "Tim"};
  BoolVar[] x;


  @Override
  public void createSolver() {
    solver = new Solver("MrSmith");
  }
  

  @Override
  public void buildModel() {    

    x = VariableFactory.boolArray("x", n, solver);
    BoolVar MrSmith  = x[0];
    BoolVar MrsSmith = x[1];
    BoolVar Matt     = x[2];
    BoolVar John     = x[3];
    BoolVar Tim      = x[4];
    
    // The comments contains encodings for other CP systems
    // (e.g. MiniZinc and or-tools/C#)


    // If Mr Smith comes then his wife will come too.
    // (Mr_Smith -> Mrs_Smith)
    // Mr_Smith - Mrs_Smith <= 0
    solver.post(IntConstraintFactory.arithm(MrSmith,"-",MrsSmith,"<=", 0));

    // At least one of their two sons Matt and John will come.
    // (Matt \/ John)
    // Matt+John >= 1
    solver.post(IntConstraintFactory.arithm(Matt,"+", John, ">=", 1));

    // Either Mrs Smith or Tim will come but not both.
    // bool2int(Mrs_Smith) + bool2int(Tim) = 1
    // (Mrs_Smith xor Tim)
    // Mrs_Smith + Tim == 1
    solver.post(IntConstraintFactory.arithm(MrsSmith,"+", Tim, "=", 1));

    // Either Tim and John will come or neither will come.
    // (Tim = John)
    solver.post(IntConstraintFactory.arithm(Tim,"=", John));

    // If Matt comes /\ then John and his father will also come.
    // (Matt -> (John /\ Mr_Smith))
    // Matt - (John*Mr_Smith) <= 0
    BoolVar JohnAndMrSmith = VariableFactory.bool("JohnAndMrSmith", solver);
    solver.post(IntConstraintFactory.times(John, MrSmith, JohnAndMrSmith));
    solver.post(IntConstraintFactory.arithm(Matt, "-", JohnAndMrSmith, "<=", 0));
  
  }
  

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }
  
  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.print("These will go: ");
        for (int i = 0; i < n; i++) {
          if (x[i].getValue() == 1) {
            System.out.print(x_s[i] + " ");
          }
        }
        System.out.println();

        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new MrSmith().execute(args);
  }


} // end class
 
