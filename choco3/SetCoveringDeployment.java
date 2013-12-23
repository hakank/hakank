/**
  *
  * Set covering deployment in Choco3.
  *    
  * See http://mathworld.wolfram.com/SetCoveringDeployment.html
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

public class SetCoveringDeployment extends AbstractProblem {


  // From http://mathworld.wolfram.com/SetCoveringDeployment.html
  String[] countries = {"Alexandria",
                        "Asia Minor",
                        "Britain",
                        "Byzantium",
                        "Gaul",
                        "Iberia",
                        "Rome",
                        "Tunis"};
  
  int n = countries.length;
  
  // the incidence matrix (neighbours)
  int[][] mat = {{0, 1, 0, 1, 0, 0, 1, 1},
                 {1, 0, 0, 1, 0, 0, 0, 0},
                 {0, 0, 0, 0, 1, 1, 0, 0},
                 {1, 1, 0, 0, 0, 0, 1, 0},
                 {0, 0, 1, 0, 0, 1, 1, 0},
                 {0, 0, 1, 0, 1, 0, 1, 1},
                 {1, 0, 0, 1, 1, 1, 0, 1},
                 {1, 0, 0, 0, 0, 1, 1, 0}};
  
  IntVar[] x;
  IntVar[] y;
  IntVar num_armies;


  @Override
  public void createSolver() {
    solver = new Solver("SetCoveringDeployment");
  }
  

  @Override
  public void buildModel() {    
    

    // First army
    x = VariableFactory.enumeratedArray("x", n, 0, 1, solver);
    // Second army
    y = VariableFactory.enumeratedArray("y", n, 0, 1, solver);

    IntVar xsum = VariableFactory.bounded("xsum", 0, n, solver);
    IntVar ysum = VariableFactory.bounded("ysum", 0, n, solver);
    // total number of armies, to be minimized
    num_armies = VariableFactory.bounded("num_armires", 0, n*2, solver);

    solver.post(IntConstraintFactory.sum(x, xsum));
    solver.post(IntConstraintFactory.sum(y, ysum));
    solver.post(IntConstraintFactory.sum(new IntVar[]{xsum, ysum}, num_armies));

    //
    //  Constraint 1: There is always an army in a city
    //                (+ maybe a backup)
    //                Or rather: Is there a backup, there
    //                must be an an army
    //
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.arithm(x[i], ">=", y[i]));
    }

    //
    // Constraint 2: There should always be an backup
    //               army near every city
    //
    /*
    forall(i in 1..n) (
          (X[i] + sum(j in 1..n where matrix[i,j] = 1) (Y[j]))  >= 1
    )
    */
    for(int i = 0; i < n; i++) {
      ArrayList<IntVar> count_neighbours = new ArrayList<IntVar>();
      for(int j = 0; j < n; j++) {
        if (mat[i][j] == 1) {
          count_neighbours.add(y[j]);
        }
      }
      IntVar csum = VariableFactory.bounded("csum", 0, n, solver);
      solver.post(IntConstraintFactory.sum(count_neighbours.toArray(new IntVar[1]), csum));
      IntVar xi_csum = VariableFactory.bounded("xi_csum", 0, n, solver);
      solver.post(IntConstraintFactory.sum(new IntVar[]{x[i], csum}, xi_csum));
      solver.post(IntConstraintFactory.arithm(xi_csum,
                                              ">=", 
                                              VariableFactory.fixed(1, solver)));
    }
      
  }
    

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }
  
  @Override
  public void solve() {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, num_armies);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("num_armies: " + num_armies.getValue());
        System.out.println("Selected armies:");
        boolean found = false;
        for (int i = 0; i < n; i++) {
          if (x[i].getValue() == 1) {
            System.out.print("Army: " + countries[i] + " ");
            found = true;
          }
          if (y[i].getValue() == 1) {
            System.out.print("Reserve army: " + countries[i] + " ");
            found = true;
          }
          if (found) {
            System.out.println();
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
    new SetCoveringDeployment().execute(args);
  }


} // end class
 
