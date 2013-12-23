/**
 *
 * Quasigroup Completion problem in Choco3.
 *
 * See 
 * Carla P. Gomes and David Shmoys:
 * "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
 *
 *
 * See also
 * Ivars Peterson "Completing Latin Squares"
 * http://www.maa.org/mathland/mathtrek_5_8_00.html
 * """
 * Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
 * a four-by-four array so that no column or row contains the same two numbers. 
 * The result is known as a Latin square.
 * ...
 * The so-called quasigroup completion problem concerns a table that is correctly 
 * but only partially filled in. The question is whether the remaining blanks in 
 * the table can be filled in to obtain a complete Latin square (or a proper 
 * quasigroup multiplication table).
 * """
 * 
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * http://www.hakank.org/choco3/
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
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.io.*;
import java.util.*;
import java.text.*;

public class QuasigroupCompletion extends AbstractProblem {

  @Option(name = "-file", usage = "instance file.", required = false)
  String file = null;


  int n;
  IntVar[][] x;
    
  int[][] matrix;

  //
  // k_all_different (make x a latin square)
  //
  public void k_all_different(IntVar[][] y, int n) {

    for(int i = 0; i < n; i++) {
      ArrayList<IntVar> col = new ArrayList<IntVar>();
      for(int j = 0; j < n; j++) {
        // rows
        solver.post(IntConstraintFactory.alldifferent(y[i],"AC"));
        col.add(y[j][i]);
      }
      // columns
      solver.post(IntConstraintFactory.alldifferent(col.toArray(new IntVar[1]), "AC"));
    }

  } // end k_all_different


  @Override 
  public void buildModel() {

    if (file == null) {

      /* Example from Ruben Martins and Inès Lynce
         Breaking Local Symmetries in Quasigroup Completion Problems, page 3
         The solution is unique:
         1 3 2 5 4
         2 5 4 1 3
         4 1 3 2 5
         5 4 1 3 2
         3 2 5 4 1
      */
      n = 5;
      matrix = new int[][] {{1, 0, 0, 0, 4},
                            {0, 5, 0, 0, 0},
                            {4, 0, 0, 2, 0},
                            {0, 4, 0, 0, 0},
                            {0, 0, 5, 0, 1}};
    } else {

      readFile(file);

    }
 

    // decision variables
    x = VariableFactory.enumeratedMatrix("x", n, n, 1, n, solver);

    // handle given hints
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (matrix[i][j] > 0) {
          solver.post(IntConstraintFactory.arithm(x[i][j], "=", matrix[i][j]));
        }
      }
    }

    // do a quasigroup completion (ensure Latin square)
    k_all_different(x, n);


  }


  @Override
  public void createSolver() {
    solver = new Solver("QuasigroupCompletion");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
  }

  @Override
  public void solve() {
    System.out.println(solver); // Solver/model before solve.
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sols = 0;
      do {
        num_sols++;
        printMatrix(x, n, n);
        System.out.println();
      } while(solver.nextSolution() == Boolean.TRUE);

      System.out.println("It was " + num_sols + " solutions.");

    }  else {

      System.out.println("No solution.");

    }



  }

  /**
   *
   * Reads a Quasigroup completion file.
   * File format:
   *  # a comment which is ignored
   *  % a comment which also is ignored
   *  number of rows (n)
   *  <
   *    row number of space separated entries
   *  >
   * 
   * "." or "0" means unknown, integer 1..n means known value
   * 
   * Example
   *   5
   *    1 . . . 4
   *   . 5 . . .
   *   4 . . 2 .
   *   . 4 . . .
   *   . . 5 . 1
   *
   */
  public void readFile(String file) {

    System.out.println("readFile(" + file + ")");
    int lineCount = 0;
        
    try {

      BufferedReader inr = new BufferedReader(new FileReader(file));
      String str;
      while ((str = inr.readLine()) != null && str.length() > 0) {

        str = str.trim();

        // ignore comments
        if(str.startsWith("#") || str.startsWith("%")) {
          continue;
        }

        System.out.println(str);
        if (lineCount == 0) {
          n = Integer.parseInt(str); // number of rows
          matrix = new int[n][n];
        } else {
          // the problem matrix
          String row[] = str.split(" ");
          for(int i = 0; i < n; i++) {
            String s = row[i];
            if (s.equals(".")) {
              matrix[lineCount-1][i] = 0;
            } else {
              matrix[lineCount-1][i] = Integer.parseInt(s);
            }
          }
        }

        lineCount++;

      } // end while

      inr.close();

    } catch (IOException e) {
      System.out.println(e);
    }

  } // end readFile
    

    //
    // prints a variable matrix
    //
  void printMatrix(IntVar[][] y, int rows, int cols) {

    for(int i = 0; i < rows; i++) {
      for(int j = 0; j < cols; j++) {
        System.out.print(y[i][j].getValue()+ " ");
      }
      System.out.println();
    }

  } // end printMatrix


  public static void main(String args[]) {

    new QuasigroupCompletion().execute(args);

  }

} // end class

