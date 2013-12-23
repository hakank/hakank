/**
 *
 * Young tableaux in Choco3.
 *
 * See 
 * http://mathworld.wolfram.com/YoungTableau.html
 * and
 * http://en.wikipedia.org/wiki/Young_tableau
 * """
 * The partitions of 4 are
 *  {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 *
 * And the corresponding standard Young tableaux are:
 *
 * 1.   1 2 3 4
 *
 * 2.   1 2 3         1 2 4    1 3 4
 *      4             3        2
 *
 * 3.   1 2           1 3
 *      3 4           2 4
 *
 * 4    1 2           1 3      1 4 
 *      3             2        2 
 *      4             4        3
 *
 * 5.   1
 *      2
 *      3
 *      4
 * """  
 * 
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * Also see http://www.hakank.org/choco3/
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
import solver.constraints.LogicalConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.io.*;
import java.util.*;
import java.text.*;

public class YoungTableuax extends AbstractProblem {

  @Option(name = "-n", usage = "n (default 6).", required = false)
  int n = 6;


  IntVar[][] x; // matrix
  IntVar[] x_a; // x as an array, for Count
  IntVar[] p; // partition structure.

  IntVar[] gcc_count; // Global constraint count

  // Decomposition of global constraint increasing
  public void increasing(IntVar[] v) {
    for(int j = 1; j < n; j++) {
      solver.post(IntConstraintFactory.arithm(v[j], ">=", v[j-1]));
    }
  }


  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedMatrix("x", n, n, 1, n+1, solver);
    x_a = ArrayUtils.flatten(x);
    p = VariableFactory.enumeratedArray("p", n, 0, n+1, solver);


    // Initialize the variables.
    //
    // Value n+1 in x will be replaced with "_" in the output.
    // This representation simplifies the "increasing" constraints below.
    //
    // 1..n is used exactly once in the whole matrix. 
    // n+1, however will be used n^2 - n times.
    //
    for(int i = 1; i <= n; i++) {
      solver.post(IntConstraintFactory.count(i, x_a, VariableFactory.fixed(1, solver)));
    }
    
    solver.post(IntConstraintFactory.arithm(x[0][0], "=", 1));

    //
    // increasing row wise
    //
    for(int i = 0; i < n; i++) {
      increasing(x[i]);
    }

    //
    // increasing column wise
    //
    for(int j = 0; j < n; j++) {
      for(int i = 1; i < n; i++) {
        solver.post(IntConstraintFactory.arithm(x[i][j], ">=", x[i-1][j]));
      }
    }

    // calculate the partition structure:

    // for each row: sum number of entries where x[i][j] between 1.. n
    // i.e. ignore those that is n+1
    for(int i = 0; i < n; i++) {
      BoolVar[] p_bin = VariableFactory.boolArray("p_bin", n, solver); // new Boolar[n];

      // sum all entries where x[i][j] <= n
      for(int j = 0; j < n; j++) {
        // reification
        solver.post(LogicalConstraintFactory.ifThenElse(p_bin[j], 
                                                    IntConstraintFactory.arithm(x[i][j],"<=", n),
                                                    IntConstraintFactory.arithm(x[i][j],">", n)));

      }

      solver.post(IntConstraintFactory.sum(p_bin, p[i]));

    }

    solver.post(IntConstraintFactory.sum(p, VariableFactory.fixed(n, solver)));

  }

  @Override
  public void createSolver() {
    solver = new Solver("YoungTableuax(" + n + ")");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x_a));
  }


  @Override
  public void solve() {
    solver.findSolution();
  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {

      do {
                
        // Tableaux
        System.out.println("\nTableaux:");
        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            int r = x[i][j].getValue();
            if (r == n+1) {
              System.out.print("_" + " ");                        
            } else {
              System.out.print(r + " ");
            }
          }
          System.out.println();
        }
                
        // Partition
        System.out.println("Partition: ");
        for(int i = 0; i < n; i++) {
          System.out.print(p[i].getValue() + " ");
        }
        System.out.println("\n");

      } while (solver.nextSolution() == Boolean.TRUE);

            

    } else {

      System.out.println("Problem is not feasible.");

    }


  } // end model


  public static void main(String args[]) {

    new YoungTableuax().execute(args);

  }
}
