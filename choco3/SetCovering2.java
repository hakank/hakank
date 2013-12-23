/**
  *
  * Set covering problem in Choco3.
  *
  * Example 9.1-2 from
  * Taha "Operations Research - An Introduction",
  * page 354ff.
  * Minimize the number of security telephones in street
  * corners on a campus.
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

public class SetCovering2 extends AbstractProblem {

  int[][] corners;
  int num_streets;
  int n;

  IntVar[] x;
  IntVar z;


  @Override
  public void createSolver() {
    solver = new Solver("SetCovering2");
  }
  

  @Override
  public void buildModel() {    

    
    n = 8;            // maximum number of corners
    num_streets = 11; // number of connected streets

    // corners of each street
    // Note: 1-based (handled below)
    corners = new int[][] {{1,2},
                           {2,3},
                           {4,5},
                           {7,8},
                           {6,7},
                           {2,6},
                           {1,6},
                           {4,7},
                           {2,4},
                           {5,8},
                           {3,5}};


    x = VariableFactory.enumeratedArray("x", n, 0, 1, solver);
    z = VariableFactory.bounded("z", 0, 10000, solver);

    // number of used telehpones, to be minimized
    solver.post(IntConstraintFactory.sum(x, z));

    // ensure that all cities are covered
    for(int i = 0; i < num_streets; i++) {
      IntVar[] b = new IntVar[2];
      b[0] = x[corners[i][0] - 1];
      b[1] = x[corners[i][1] - 1];
      IntVar ssum = VariableFactory.bounded("ssum", 0, 2, solver);
      solver.post(IntConstraintFactory.sum(b, ssum));
      solver.post(IntConstraintFactory.arithm(ssum,">=", VariableFactory.fixed(1, solver)));
    }

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }
  
  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("z: " + z.getValue());
        for (int i = 0; i < n; i++) {
            System.out.print(x[i].getValue() + " ");
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
    new SetCovering2().execute(args);
  }


} // end class
 
