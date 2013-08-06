/**
 *
 * Bales of Hay problem in Choco3.
 *
 * Yet another weighing problem.
 *
 * From The Math Less Traveled, 
 * "The haybaler", http://www.mathlesstraveled.com/?p=582 
 * """
 * You have five bales of hay.
 *
 * For some reason, instead of being weighed individually, they were weighed 
 * in all possible combinations of two. The weights of each of these 
 * combinations were written down and arranged in numerical order, without 
 * keeping track of which weight matched which pair of bales. The weights, 
 * in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.
 *
 * How much does each bale weigh? Is there a solution? Are there multiple 
 * possible solutions? 
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
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class BalesOfHay extends AbstractProblem {
  
  int n = 5;
  int m = 50;
  int[] weights = {80, 82, 83, 84, 85, 86, 87, 88, 90, 91};

  IntVar[] x;

  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedArray("x", n, 0, m, solver);

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    for(int w = 0; w < weights.length; w++) {

      // bales[i] + bales[j] == weights[w]
      IntVar i = VariableFactory.enumerated("i", 0, n-1, solver);
      IntVar j = VariableFactory.enumerated("j", 0, n-1, solver);
      IntVar balesI = VariableFactory.bounded("balesI", 0, m, solver);
      IntVar balesJ = VariableFactory.bounded("balesJ", 0, m, solver);

      solver.post(IntConstraintFactory.element(balesI, x, i, 0));
      solver.post(IntConstraintFactory.element(balesJ, x, j, 0));

      solver.post(IntConstraintFactory.arithm(balesI,"+",balesJ, "=", weights[w]));
      
      // symmetry breaking
      solver.post(IntConstraintFactory.arithm(i, "<", j));

    }

    // symmetry breaking
    for(int i = 1; i < n; i++) {
      solver.post(IntConstraintFactory.arithm(x[i-1], "<", x[i]));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("BalesOfHay");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(x, seed));
  }

  @Override
  public void solve() {
      solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {

        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
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

    new BalesOfHay().execute(args);

  }

}

