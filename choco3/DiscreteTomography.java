/**
  *
  * Discrete tomography in Choco3.
  *
  * Problem from http://eclipse-clp.org/examples/tomo.ecl.txt
  * """
  * This is a little "tomography" problem, taken from an old issue
  * of Scientific American.
  *
  * A matrix which contains zeroes and ones gets "x-rayed" vertically and
  * horizontally, giving the total number of ones in each row and column.
  * The problem is to reconstruct the contents of the matrix from this
  * information. Sample run:
  *
  * ?- go.
  *    0 0 7 1 6 3 4 5 2 7 0 0
  * 0                         
  * 0                         
  * 8      * * * * * * * *    
  * 2      *             *    
  * 6      *   * * * *   *    
  * 4      *   *     *   *    
  * 5      *   *   * *   *    
  * 3      *   *         *    
  * 7      *   * * * * * *    
  * 0                         
  * 0                         
  *
  *
  * Eclipse solution by Joachim Schimpf, IC-Parc
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class DiscreteTomography extends AbstractProblem {


  @Option(name = "-d", aliases = "--data", usage = "Problem instance", required = false)
  Data data = Data.p1;


  int r;  // number of rows
  int c;  // number of columns
  int[] row_sums;
  int[] col_sums;
 
  IntVar[][] x;


  @Override
  public void createSolver() {
    solver = new Solver("DiscreteTomography");
  }
  

  @Override
  public void buildModel() {    

    row_sums = data.getRowSums();
    col_sums = data.getColSums();

    r = row_sums.length;
    c = col_sums.length;

    x = VariableFactory.enumeratedMatrix("x", r, c, 0, 1, solver); 

    for(int i = 0; i < r; i++) {
      solver.post(IntConstraintFactory.sum(x[i], VariableFactory.fixed(row_sums[i], solver)));
    }

    for(int j = 0; j < c; j++) {
      solver.post(IntConstraintFactory.sum(ArrayUtils.getColumn(x, j), VariableFactory.fixed(col_sums[j], solver)));
    }
  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
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
        for (int i = 0; i < r; i++) {
          for (int j = 0; j < c; j++) {
            if (x[i][j].getValue() == 1) {
              System.out.print("* ");
            } else {
              System.out.print("  ");
            }
          }
          System.out.println();
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

    new DiscreteTomography().execute(args);

  }

  static enum Data {

    // p1, p2, and p3 are from the ECLiPSe model cited above.
    p1(
       new int[][] {
         {0,0,8,2,6,4,5,3,7,0,0}, // row_sums
         {0,0,7,1,6,3,4,5,2,7,0,0}, // col_sums
       }),
      
      p2(
         new int[][] {
           {10,4,8,5,6},
           {5,3,4,0,5,0,5,2,2,0,1,5,1}
         }),
      
      // This give three slightly different solutions.
      p3(
         new int[][] {
           {11,5,4},
           {3,2,3,1,1,1,1,2,3,2,1}
         }
         ),

      // My own problem instance
      p4(
         new int[][] {
           {0,2,2,2,2,2,8,8,4,4,4,4,4,0},
           {0,0,0,12,12,2,2,2,2,7,7,0,0,0}
         }
         );

    final int[][] data;
    
    Data(int[][] data) {
      this.data = data;
    }
    
    int[] getRowSums() {
      return data[0];
    }
    
    int[] getColSums() {
      return data[1];
    }
 
  }

} // end class
 
