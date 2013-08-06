/**
 *
 * Strimko solver in Choco3.
 *
 * From 
 * 360: A New Twist on Latin Squares
 * http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
 * """
 * The idea is simple: each row and column of an nxn grid must contain 
 * the number 1, 2, ... n exactly once (that is, the grid must form a 
 * Latin square), and each "stream" (connected path in the grid) must 
 * also contain the numbers 1, 2, ..., n exactly once.
 * """
 *
 * For more information, see:
 * * http://www.strimko.com/
 * * http://www.strimko.com/rules.htm
 * * http://www.strimko.com/about.htm
 * * http://www.puzzlersparadise.com/Strimko.htm
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

public class Strimko2 extends AbstractProblem {

  int[][] streams;
  int[][] placed;

  int n;
  int num_placed;

  IntVar[][] x;


  @Override
  public void createSolver() {
    solver = new Solver("Strimko2");
  }
  

  @Override
  public void buildModel() {    
    

    //
    // data
    //
    streams = new int[][] {{1,1,2,2,2,2,2},
                           {1,1,2,3,3,3,2},
                           {1,4,1,3,3,5,5},
                           {4,4,3,1,3,5,5},
                           {4,6,6,6,7,7,5},
                           {6,4,6,4,5,5,7},
                           {6,6,4,7,7,7,7}};

    // Note: This is 1-based
    placed = new int[][] {{2,1,1},
                          {2,3,7},
                          {2,5,6},
                          {2,7,4},
                          {3,2,7},
                          {3,6,1},
                          {4,1,4},
                          {4,7,5},
                          {5,2,2},
                          {5,6,6}};

    n = streams.length;
    num_placed = placed.length;


    x = VariableFactory.enumeratedMatrix("x", n, n, 1, n, solver);
    

    // all rows and columns must be unique, i.e. a Latin Square
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.alldifferent(x[i], "BC"));
      solver.post(IntConstraintFactory.alldifferent(ArrayUtils.getColumn(x, i), "BC"));
    }

   // streams
    for(int s = 1; s <= n; s++) {
      ArrayList<IntVar> tmp = new ArrayList<IntVar>();
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          if (streams[i][j] == s) {
            tmp.add(x[i][j]);
          }
        }
      }
      solver.post(IntConstraintFactory.alldifferent(tmp.toArray(new IntVar[1]),"BC"));
    }


    // placed
    for(int i = 0; i <  num_placed; i++) {
      // note: also adjust to 0-based
      solver.post(IntConstraintFactory.arithm(
                                              x[placed[i][0] - 1][placed[i][1] - 1],
                                              "=",
                                              placed[i][2]));
    }

    
  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(ArrayUtils.flatten(x)));
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
        for (int i = 0; i < n; i++) {
          for (int j = 0; j < n; j++) {
            System.out.print(x[i][j].getValue() + " ");
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
    new Strimko2().execute(args);
  }


} // end class
 
