/**
  *
  * Ski assignment problem in Choco3.
  *
  * From
  * Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008 – Final Review, December 12, 2008
  * http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  * """
  * 5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  * decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  * They'll let you ski all winter for free in exchange for helping their ski rental 
  * shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  * obtain a pair of skis whose height matches his or her own height exactly. 
  * Unfortunately, this is generally not possible. We define the disparity between 
  * a skier and his or her skis to be the absolute value of the difference between 
  * the height of the skier and the pair of skis. Our objective is to find an 
  * assignment of skis to skiers that minimizes the sum of the disparities. 
  * ...
  * Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  * following sample data:
  *   * Ski heights: 1, 2, 5, 7, 13, 21.
  *   * Skier heights: 3, 4, 7, 11, 18.
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

public class SkiAssignment extends AbstractProblem {

  int[] ski_heights;
  int[] skier_heights;
  int num_skis;
  int num_skiers;

  IntVar[] x;
  IntVar z;

  @Override
  public void createSolver() {
    solver = new Solver("SkiAssignment");
  }
  

  @Override
  public void buildModel() {    

    ski_heights = new int[] {1, 2, 5, 7, 13, 21};
    skier_heights = new int[] {3, 4, 7, 11, 18};

    num_skis = ski_heights.length;
    num_skiers = skier_heights.length;

    x = VariableFactory.enumeratedArray("x", num_skiers, 1, num_skis, solver); 
    z = VariableFactory.bounded("z", 0, 100, solver); 

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    // MiniZinc code: z = sum(i in 1..num_skiers) ( abs(ski_heights[x[i]] - skier_heights[i]) );
    IntVar[] diffs = new IntVar[num_skiers];
    for(int i = 0; i < num_skiers; i++) {
      IntVar s = VariableFactory.bounded("s_"+i, 0, 30, solver);
      solver.post(IntConstraintFactory.element(s, ski_heights, x[i], 0, "detect"));
      diffs[i] = VariableFactory.abs(VariableFactory.offset(s, -skier_heights[i]));
    }

    solver.post(IntConstraintFactory.sum(diffs, z));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
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
        for (int i = 0; i < num_skiers; i++) {
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

    new SkiAssignment().execute(args);

  }


}
 
