/**
  *
  * Broken weights problem in Choco3.
  *
  * From http://www.mathlesstraveled.com/?p=701
  * """
  * Here's a fantastic problem I recently heard. Apparently it was first
  * posed by Claude Gaspard Bachet de Meziriac in a book of arithmetic problems
  * published in 1612, and can also be found in Heinrich Dorrie's 100
  * Great Problems of Elementary Mathematics.
  *
  *   A merchant had a forty pound measuring weight that broke
  *   into four pieces as the result of a fall. When the pieces were
  *   subsequently weighed, it was found that the weight of each piece
  *   was a whole number of pounds and that the four pieces could be
  *   used to weigh every integral weight between 1 and 40 pounds. What
  *   were the weights of the pieces?
  *
  * Note that since this was a 17th-century merchant, he of course used a
  * balance scale to weigh things. So, for example, he could use a 1-pound
  * weight and a 4-pound weight to weigh a 3-pound object, by placing the
  * 3-pound object and 1-pound weight on one side of the scale, and
  * the 4-pound weight on the other side.
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

public class BrokenWeights extends AbstractProblem {

  @Option(name = "-m", usage = "weight (default 40).", required = false)
  int m = 40;

  @Option(name = "-n", usage = "number of pieces (default 4).", required = false)
  int n = 4;


  IntVar[][] x;
  IntVar[] weights;


  @Override
  public void createSolver() {
    solver = new Solver("BrokenWeights");
  }
  

  @Override
  public void buildModel() {    

    weights = VariableFactory.enumeratedArray("weights", n, 1, m, solver);
    x = VariableFactory.enumeratedMatrix("x", m, n, -1, 1, solver);


    // The selected weights sums to m
    solver.post(IntConstraintFactory.sum(weights, VariableFactory.fixed(m, solver)));

    //
    // Check that all weights from 1 to n (default 40) can be made.
    //
    // Since all weights can be on either side
    // of the side of the scale we allow either
    // -1, 0, or 1 of the weights, assuming that
    // -1 is the weights on the left and 1 is on the right.
    //
    // Note: we checks for i+1.
    for(int i = 0; i < m; i++) {
      IntVar[] s = VariableFactory.enumeratedArray("s", n, -m, m, solver);
      for(int j = 0; j < n; j++) {
        solver.post(IntConstraintFactory.times(x[i][j], weights[j], s[j]));
      }
      solver.post(IntConstraintFactory.sum(s,VariableFactory.fixed(i+1, solver)));
    }

    // symmetry breaking: order the weights
    for(int j = 1; j < n; j++) {
      solver.post(IntConstraintFactory.arithm(weights[j-1],"<",weights[j]));
    }

  }
  

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(ArrayUtils.append(weights, ArrayUtils.flatten(x))));
  }
  
  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, weights[n-1]);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.print("weights: ");
        for (int i = 0; i < n; i++) {
          System.out.format("%3d ", weights[i].getValue());
        }
        System.out.println("\nx:");
        for (int i = 0; i < m; i++) {
          System.out.format("%3d: ", i+1);
          for (int j = 0; j < n; j++) {
            System.out.format("%3d ", x[i][j].getValue());
          }
          System.out.println();
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
    new BrokenWeights().execute(args);
  }


} // end class
 
