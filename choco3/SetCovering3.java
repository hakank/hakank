/**
  *
  * Set covering problem in Choco3.
  *    
  * Set covering problem from
  * Katta G. Murty: 'Optimization Models for Decision Making',
  * page 302f
  * http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
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

public class SetCovering3 extends AbstractProblem {

  @Option(name = "-show_all_opt", usage = "Show all optimal solutions (default false).", required = false)
  boolean show_all_opt = false;


  int[][] belongs;
  int num_groups;
  int num_senators;

  IntVar[] x;
  IntVar z;


  @Override
  public void createSolver() {
    solver = new Solver("SetCovering3");
  }
  

  @Override
  public void buildModel() {    

    num_groups = 6;
    num_senators = 10;

    // which group does a senator belong to?
    belongs = new int[][] {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0},   // 1 southern
                           {0, 0, 0, 0, 0, 1, 1, 1, 1, 1},   // 2 northern
                           {0, 1, 1, 0, 0, 0, 0, 1, 1, 1},   // 3 liberals
                           {1, 0, 0, 0, 1, 1, 1, 0, 0, 0},   // 4 conservative
                           {0, 0, 1, 1, 1, 1, 1, 0, 1, 0},   // 5 democrats
                           {1, 1, 0, 0, 0, 0, 0, 1, 0, 1}};  // 6 republicans
    
    x = VariableFactory.enumeratedArray("x", num_senators, 0, 1, solver);
    z = VariableFactory.bounded("z", 0, 1000, solver);

    // number of assigned senators, to be minimized
    solver.post(IntConstraintFactory.sum(x, z));

    // ensure that each group is covered by at least
    // one senator
    for(int i = 0; i < num_groups; i++) {
      IntVar[] b = VariableFactory.boolArray("b", num_senators, solver); // new IntVar[num_senators];
      for(int j = 0; j < num_senators; j++) {
        solver.post(IntConstraintFactory.times(x[j], 
                                               VariableFactory.fixed(belongs[i][j],solver), 
                                               b[j]));
      }
      IntVar ssum = VariableFactory.bounded("ssum", 0, num_senators, solver);
      solver.post(IntConstraintFactory.sum(b, ssum));
      solver.post(IntConstraintFactory.arithm(ssum,">=", VariableFactory.fixed(1, solver)));

    }

    if (show_all_opt) {
      solver.post(IntConstraintFactory.arithm(z,"=", VariableFactory.fixed(2, solver)));
    }

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }
  
  @Override
  public void solve() {
    if (show_all_opt) {
      solver.findSolution();
    } else {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
    }

  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("z: " + z.getValue());
        for (int i = 0; i < num_senators; i++) {
            System.out.print(x[i].getValue() + " ");
        }
        System.out.println();

        System.out.print("Selected senators: ");
        for (int i = 0; i < num_senators; i++) {
          if (x[i].getValue() == 1) {
            System.out.print(i + " ");
          }
        }
        System.out.println("\n");

        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new SetCovering3().execute(args);
  }


} // end class
 
