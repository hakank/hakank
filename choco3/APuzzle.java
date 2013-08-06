/**
 *
 * "A puzzle" in Choco3.
 *
 * "A puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
 * And the sequel "Answer to a puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/
 *
 * This problem instance was taken from the latter blog post.
 * (Problem 1)
 *
 * """
 * 8809 = 6
 * 7111 = 0
 * 2172 = 0
 * 6666 = 4
 * 1111 = 0
 * 3213 = 0
 * 7662 = 2
 * 9312 = 1
 * 0000 = 4
 * 2222 = 0
 * 3333 = 0
 * 5555 = 0
 * 8193 = 3
 * 8096 = 5
 * 7777 = 0
 * 9999 = 4
 * 7756 = 1
 * 6855 = 3
 * 9881 = 5
 * 5531 = 0
 *
 * 2581 = ?
 * """
 *
 * Note:
 * This model yields 10 solutions, since x4 is not
 * restricted in the constraints.
 * All solutions has x assigned to the correct result.
 *
 *
 * (Problem 2)
 * The problem stated in "A puzzle"
 * http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
 * is
 * """
 * 8809 = 6
 * 7662 = 2
 * 9312 = 1
 * 8193 = 3
 * 8096 = 5
 * 7756 = 1
 * 6855 = 3
 * 9881 = 5
 *
 * 2581 = ?
 * """
 * This problem instance yields two different solutions of x,
 * one is the same (correct) as for the above problem instance,
 * and one is not.
 * This is because here x0,x1,x4 and x9 are underdefined.
 *
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
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.selectors.values.InDomainMax;
import solver.search.strategy.selectors.values.InDomainMiddle;
import solver.search.strategy.selectors.values.InDomainMin;
import solver.search.strategy.selectors.values.InDomainRandom;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import solver.search.strategy.strategy.Assignment;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class APuzzle extends AbstractProblem {

  @Option(name = "-p", usage = "Problem instance (default 1).", required = false)
  int p = 1;

  int n = 10;

  int[][] problem1 = {{8,8,0,9, 6},
                      {7,1,1,1, 0},
                      {2,1,7,2, 0},
                      {6,6,6,6, 4},
                      {1,1,1,1, 0},
                      {3,2,1,3, 0},
                      {7,6,6,2, 2},
                      {9,3,1,2, 1},
                      {0,0,0,0, 4},
                      {2,2,2,2, 0},
                      {3,3,3,3, 0},
                      {5,5,5,5, 0},
                      {8,1,9,3, 3},
                      {8,0,9,6, 5},
                      {7,7,7,7, 0},
                      {9,9,9,9, 4},
                      {7,7,5,6, 1},
                      {6,8,5,5, 3},
                      {9,8,8,1, 5},
                      {5,5,3,1, 0}};
  
  int[][] problem2 = {{8,8,0,9, 6},
                      {7,6,6,2, 2},
                      {9,3,1,2, 1},
                      {8,1,9,3, 3},
                      {8,0,9,6, 5},
                      {7,7,5,6, 1},
                      {6,8,5,5, 3},
                      {9,8,8,1, 5}};
  int[][] problem;


  IntVar x0,x1,x2,x3,x4,x5,x6,x7,x8,x9;
  IntVar x;
  IntVar[] all;

  @Override
  public void buildModel() {

    if (p == 2) {     
      problem = problem2;
    } else {
      problem = problem1;
    }

    x0 = VariableFactory.bounded("x0", 0, n-1, solver);
    x1 = VariableFactory.bounded("x1", 0, n-1, solver);
    x2 = VariableFactory.bounded("x2", 0, n-1, solver);
    x3 = VariableFactory.bounded("x3", 0, n-1, solver);
    x4 = VariableFactory.bounded("x4", 0, n-1, solver);
    x5 = VariableFactory.bounded("x5", 0, n-1, solver);
    x6 = VariableFactory.bounded("x6", 0, n-1, solver);
    x7 = VariableFactory.bounded("x7", 0, n-1, solver);
    x8 = VariableFactory.bounded("x8", 0, n-1, solver);
    x9 = VariableFactory.bounded("x9", 0, n-1, solver);

    all = new IntVar[] {x0,x1,x2,x3,x4,x5,x6,x7,x8,x9};

    // The unknown, i.e 2581 = x
    x = VariableFactory.bounded("x", 0, n-1, solver);
   

    for(int i = 0; i < problem.length; i++) {
      IntVar[] segment = VariableFactory.enumeratedArray("segment_"+i, 4, 0, n-1, solver);
      for(int j = 0; j < 4; j++) {
        segment[j] = all[problem[i][j]];
      }
      solver.post(IntConstraintFactory.sum(segment, VariableFactory.fixed(problem[i][4], solver)));
    }

    // solve "x"
    solver.post(IntConstraintFactory.sum(new IntVar[] {all[2],all[5],all[8],all[1]}, x));


  }


  @Override
  public void createSolver() {
    solver = new Solver("APuzzle");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(all)); 
  }

  @Override
  public void solve() {
    solver.findSolution();
    // solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.print("x: " + x.getValue() + " x0..x9: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", all[i].getValue());
        }
        System.out.println();
          
        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new APuzzle().execute(args);

  }

}

