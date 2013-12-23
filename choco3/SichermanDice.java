/**
 *
 * Sicherman Dice in Choco3.
 *
 * From http://en.wikipedia.org/wiki/Sicherman_dice
 *  ""
 * Sicherman dice are the only pair of 6-sided dice which are not normal dice,
 * bear only positive integers, and have the same probability distribution for
 * the sum as normal dice.
 * 
 * The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
 *  ""
 * 
 * I read about this problem in a book/column by Martin Gardner long
 * time ago, and got inspired to model it now by the WolframBlog post
 * "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/
 * 
 * This model gets the two different ways, first the standard way and
 * then the Sicherman dice:
 * 
 *  x1 = [1, 2, 3, 4, 5, 6]
 *  x2 = [1, 2, 3, 4, 5, 6]
 *  ----------
 *  x1 = [1, 2, 2, 3, 3, 4]
 *  x2 = [1, 3, 4, 5, 6, 8]
 *
 * 
 * Extra: If we also allow 0 (zero) as a valid value then the
 * following two solutions are also valid:
 * 
 *  x1 = [0, 1, 1, 2, 2, 3]
 *  x2 = [2, 4, 5, 6, 7, 9]
 *  ----------
 *  x1 = [0, 1, 2, 3, 4, 5]
 *  x2 = [2, 3, 4, 5, 6, 7]
 * 
 * These two extra cases are mentioned here:
 * http://mathworld.wolfram.com/SichermanDice.html
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
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.constraints.LogicalConstraintFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class SichermanDice extends AbstractProblem {

  @Option(name = "-lowest_value", usage = "Lowest value of a die (default 1).", required = false)
  int lowest_value = 1;

  int n = 6;
  int m = 10;

  int[] standard_dist = {1,2,3,4,5,6,5,4,3,2,1};

  IntVar[] x1;
  IntVar[] x2;


  @Override
  public void buildModel() {

    x1 = VariableFactory.enumeratedArray("x1", n, lowest_value, m, solver);
    x2 = VariableFactory.enumeratedArray("x2", n, lowest_value, m, solver);
    

    int len = standard_dist.length;
    for(int k = 0; k < len; k++) {
      BoolVar[][] b = VariableFactory.boolMatrix("b", n, n, solver);
      IntVar k2 = VariableFactory.fixed(-(k+2), solver);
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          IntVar ss = VariableFactory.enumerated("ss", -2*m, 2*m, solver);
          solver.post(IntConstraintFactory.sum(new IntVar[] {x1[i],x2[j],k2}, ss));
          solver.post(LogicalConstraintFactory.ifThenElse(b[i][j],
                                                        IntConstraintFactory.arithm(ss,"=",0),
                                                        IntConstraintFactory.arithm(ss,"!=",0)));

        }
      }

      solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(b), VariableFactory.fixed(standard_dist[k], solver)));

    }
      
    // symmetry breaking
    for(int i = 0; i < n-1; i++) {
      solver.post(IntConstraintFactory.arithm(x1[i], "<=", x1[i+1])); // increasing x1
      solver.post(IntConstraintFactory.arithm(x2[i], "<=", x2[i+1])); // increasing x2
      solver.post(IntConstraintFactory.arithm(x1[i], "<=", x2[i]));   // x1 <= x2
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("SichermanDice");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.maxReg_InDomainMin(ArrayUtils.append(x1,x2)));
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
        System.out.print("x1: ");
        for(int i = 0; i < n; i++) {
          System.out.print(x1[i].getValue() + " ");
        }
        System.out.print("\nx2: ");
        for(int i = 0; i < n; i++) {
          System.out.print(x2[i].getValue() + " ");
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

    new SichermanDice().execute(args);

  }


}

