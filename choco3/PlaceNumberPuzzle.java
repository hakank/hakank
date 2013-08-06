/**
  *
  * Place number puzzle in Choco3.
  *
  *
  * From 
  * http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  * """
  * Place numbers 1 through 8 on nodes
  * - each number appears exactly once
  * - no connected nodes have consecutive numbers
  *       2 - 5
  *     / | X |                                 \
  *   1 - 3 - 6 - 8
  *     \ | X | /
  *       4 - 7
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
import solver.constraints.propagators.extension.nary.IterTuplesTable;
import solver.constraints.propagators.extension.nary.LargeRelation;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class PlaceNumberPuzzle extends AbstractProblem {

  int m = 32;
  int n = 8;

  // Note: this is 1-based for compatibility (and lazyness)
  //       Fixed below.
  int[][] graph =  {{1,2},
                    {1,3},
                    {1,4},
                    {2,1},
                    {2,3},
                    {2,5},
                    {2,6},
                    {3,2},
                    {3,4},
                    {3,6},
                    {3,7},
                    {4,1},
                    {4,3},
                    {4,6},
                    {4,7},
                    {5,2},
                    {5,3},
                    {5,6},
                    {5,8},
                    {6,2},
                    {6,3},
                    {6,4},
                    {6,5},
                    {6,7},
                    {6,8},
                    {7,3},
                    {7,4},
                    {7,6},
                    {7,8},
                    {8,5},
                    {8,6},
                    {8,7}};



  IntVar[] x;


  @Override
  public void createSolver() {
    solver = new Solver("PlaceNumberPuzzle");
  }
  

  @Override
  public void buildModel() {    

    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);    

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));   

    // forall(i in 1..m) (
    //    abs(x[graph[i,1]]-x[graph[i,2]]) > 1 
    //  )
    IntVar one = VariableFactory.fixed(1, solver);
    for(int i = 0; i < m; i++) {
      // (also fix to 0-base)
      solver.post(IntConstraintFactory.distance(x[graph[i][0]-1], x[graph[i][1]-1], ">", 1));
                                       
    }
    
    // symmetry breaking
    solver.post(IntConstraintFactory.arithm(x[0], "<", x[n-1]));
                

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

    new PlaceNumberPuzzle().execute(args);

  }


}
 
