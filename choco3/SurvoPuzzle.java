/**
 *
 * Survo puzzle in Choco3..
 *
 * http://en.wikipedia.org/wiki/Survo_Puzzle
 * """
 * Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
 * by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
 * Survo system which is a general environment for statistical computing and 
 * related areas.
 * 
 * In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
 * that each of these numbers appears only once and their row and column sums are 
 * equal to integers given on the bottom and the right side of the table. 
 * Often some of the integers are given readily in the table in order to 
 * guarantee uniqueness of the solution and/or for making the task easier.
 * """
 * 
 * See also
 * http://www.survo.fi/english/index.html
 * http://www.survo.fi/puzzles/index.html
 *
 * References:
 * - Mustonen, S. (2006b). "On certain cross sum puzzles"
 *   http://www.survo.fi/papers/puzzles.pdf 
 * - Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
 *   http://www.survo.fi/papers/enum_survo_puzzles.pdf 
 * - Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
 *   http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
 *
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * See also http://www.hakank.org/choco3/
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
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.io.*;
import java.util.*;
import java.text.*;

public class SurvoPuzzle extends AbstractProblem {

  @Option(name = "-file", usage = "instance file.", required = false)
  String file = null;

  int r;          // number of rows
  int c;          // number of column
  int[] rowsums;  // row sums
  int[] colsums;  // col sums
  int[][] matrix; // the clues matrix

  IntVar[][] x;      // the solution

  @Override
  public void buildModel() {

    if (file == null) {

      System.out.println("Using the default problem.");

      /* Default problem:
       *
       * http://www.survo.fi/puzzles/280708.txt, the third puzzle
       * Survo puzzle 128/2008 (1700) #364-35846
       */
      r = 3;
      c = 6;
      rowsums = new int[] {30, 86, 55};
      colsums = new int[] {22, 11, 42, 32, 27, 37};
      matrix = new int[][] {{0, 0,  0, 0, 0, 0},
                            {0, 0, 18, 0, 0, 0},
                            {0, 0,   0, 0, 0, 0}};

    } else {

      readFile(file);

    }

    // initiate structures and variables
    x = VariableFactory.enumeratedMatrix("x", r, c, 1, r*c, solver);
    for(int i = 0; i < r; i++) {
      for(int j = 0; j < c; j++) {
        if (matrix[i][j] > 0) {
          solver.post(IntConstraintFactory.arithm(x[i][j], "=", matrix[i][j]));
        }
      }
    }

    // row sums
    for(int i = 0; i < r; i++) {
      IntVar r_tmp = VariableFactory.fixed(rowsums[i], solver);
      solver.post(IntConstraintFactory.sum(x[i], r_tmp));
    }


    // column sums
    for(int j = 0; j < c; j++) { 
      ArrayList<IntVar> cols = new ArrayList<IntVar>();
      for(int i = 0; i < r; i++) {
        cols.add(x[i][j]);
      }
      IntVar c_tmp = VariableFactory.fixed(colsums[j], solver);
      solver.post(IntConstraintFactory.sum(cols.toArray(new IntVar[1]), c_tmp));
    }

    solver.post(IntConstraintFactory.alldifferent(ArrayUtils.flatten(x), "BC"));

  }


  @Override
  public void createSolver() {
    solver = new Solver("SurvoPuzzle");
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

    if(solver.isFeasible() == ESat.TRUE) {
      int num_sols = 0;
      do {
                
        for(int i = 0; i < r; i++) {
          for(int j = 0; j < c; j++) {
            System.out.print(x[i][j].getValue()+ " ");
          }
          System.out.println();
        }

        num_sols++;
                
      } while(solver.nextSolution() == Boolean.TRUE);

      System.out.println("\nNumber of solutions: " + num_sols);

    } else {

      System.out.println("Problem is not feasible.");
    }
        
  }

    
  /**
   *
   * readFile()
   *
   * Reads a Survo puzzle in the following format
   * 
   * % From http://www.survo.fi/puzzles/280708.txt
   * % Survo puzzle 128/2008 (1700) #364-35846
   * A  B  C  D  E  F
   * 1  *  *  *  *  *  * 30
   * 2  *  * 18  *  *  * 86
   * 3  *  *  *  *  *  * 55
   * 22 11 42 32 27 37
   *
   */
  public void readFile(String file) {

    System.out.println("readFile(" + file + ")");

    try {

      BufferedReader inr = new BufferedReader(new FileReader(file));
      String str;
      int lineCount = 0;
      ArrayList<ArrayList<Integer>> MatrixI = new ArrayList<ArrayList<Integer>>();
      while ((str = inr.readLine()) != null && str.length() > 0) {
                
        str = str.trim();
                
        // ignore comments
        // starting with either # or %
        if(str.startsWith("#") || str.startsWith("%")) {
          continue;
        }

        str = str.replace("_", "");                                
        String row[] = str.split("\\s+");
        System.out.println(str);

        // first line: column names: Ignore but count them
        if (lineCount == 0) {
          c = row.length;
          colsums = new int[c];
        } else  {

          // This is the last line: the column sums
          if (row.length == c) {
            colsums = new int[row.length];
            for(int j = 0; j < row.length; j++) {
              colsums[j] = Integer.parseInt(row[j]);
            }
            System.out.println();
          } else {
            // Otherwise:
            // The problem matrix: index 1 .. row.length-1
            // The row sums: index row.length
            ArrayList<Integer> this_row = new ArrayList<Integer>();
            for(int j = 0; j < row.length; j++) {
              String s = row[j];
              if (s.equals("*")) {
                this_row.add(0);
              } else {
                this_row.add(Integer.parseInt(s));
              }
            }
            MatrixI.add(this_row);
          }
                   
        }
                
        lineCount++;

      } // end while

      inr.close();

      //
      // Now we know everything to be known:
      // Construct the problem matrix and column sums.
      //
      r = MatrixI.size();
      rowsums = new int[r];
      matrix = new int[r][c];
      for(int i = 0; i < r; i++) {
        ArrayList<Integer> this_row = MatrixI.get(i);
        for(int j = 1; j < c + 1 ; j++) {
          matrix[i][j-1] = this_row.get(j);
        }
        rowsums[i] = this_row.get(c+1);
      }
            
            
    } catch (IOException e) {
      System.out.println(e);
    }
        
  } // end readFile


  public static void main(String args[]) {

    new SurvoPuzzle().execute(args);

  }


} // end class

