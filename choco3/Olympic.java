/**
  *
  * Olympic puzzle in Choco3.
  *
  * Benchmark for Prolog (BProlog)
  * """
  * File   : olympic.pl
  * Author : Neng-Fa ZHOU
  * Date   : 1993
  *
  * Purpose: solve a puzzle taken from Olympic Arithmetic Contest
  *
  * Given ten variables with the following configuration:
  *
  *                 X7   X8   X9   X10
  *
  *                    X4   X5   X6
  *
  *                       X2   X3
  *
  *                          X1
  *
  * We already know that X1 is equal to 3 and want to assign each variable
  * with a different integer from {1,2,...,10} such that for any three
  * variables
  *                        Xi   Xj
  *
  *                           Xk
  *
  * the following constraint is satisfied:
  *
  *                      |Xi-Xj| = Xk
  * """
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
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Olympic extends AbstractProblem {

  int n = 10;

  IntVar[] x;


  @Override
  public void createSolver() {
    solver = new Solver("Olympic");
  }
  

  @Override
  public void buildModel() {    

    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);
    IntVar X1  = x[0];
    IntVar X2  = x[1];
    IntVar X3  = x[2];
    IntVar X4  = x[3];
    IntVar X5  = x[4];
    IntVar X6  = x[5];
    IntVar X7  = x[6];
    IntVar X8  = x[7];
    IntVar X9  = x[8];
    IntVar X10 = x[9];

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    solver.post(IntConstraintFactory.arithm(X1,"=", VariableFactory.fixed(3, solver)));
    
    solver.post(IntConstraintFactory.distance(X2,X3,"=",X1));
    solver.post(IntConstraintFactory.distance(X4,X5,"=",X2));
    solver.post(IntConstraintFactory.distance(X5,X6,"=",X3));
    solver.post(IntConstraintFactory.distance(X7,X8,"=",X4));
    solver.post(IntConstraintFactory.distance(X8,X9, "=",X5));
    solver.post(IntConstraintFactory.distance(X9,X10,"=",X6));

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
        System.out.print("x: ");
        for (int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.println("\n");

        System.out.format("%2d  %2d  %2d  %2d\n", x[6].getValue(),x[7].getValue(),x[8].getValue(),x[9].getValue()); 
        System.out.format(" %2d  %2d  %2d\n", x[3].getValue(),x[4].getValue(),x[5].getValue()); 
        System.out.format("   %2d  %2d\n", x[1].getValue(),x[2].getValue()); 
        System.out.format("     %2d\n", x[0].getValue()); 
        System.out.println();

        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new Olympic().execute(args);
  }


} // end class
 
