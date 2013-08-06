/**
 *
 * SEND+MORE=MONEY in "any" base in Choco3.
 *
 * Examples:
 * Base 10 (the traditional) has one solution:
 *    [9, 5, 6, 7, 1, 0, 8, 2]
 *
 * Base 11 has three solutions:
 *    [10, 5, 6, 8, 1, 0, 9, 2]
 *    [10, 6, 7, 8, 1, 0, 9, 3]
 *    [10, 7, 8, 6, 1, 0, 9, 2]
 * 
 * The number of solutions for base 10 to 30 is:
 *
 *   1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,
 *
 * which is the triangular number sequence:
 * http://www.research.att.com/~njas/sequences/?q=1+3+6+10+15+21+28+36+45+55+66+&language=english&go=Search
 *
 * I blogged about this relation in
 * "Some other Gecode/R models, mostly recreational mathematics"
 * http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html
 *
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
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class SendMoreMoneyAnyBase extends AbstractProblem {

  @Option(name = "-base", usage = "base (default 10).", required = false)
  int base = 10;

  int base1;

  String[] x_s = {"s","e","n","d","m","o","r","y"};

  IntVar[] x;


  @Override
  public void buildModel() {

    base1 = base-1;

    IntVar S = VariableFactory.enumerated("s", 0, base1, solver);
    IntVar E = VariableFactory.enumerated("e", 0, base1, solver);
    IntVar N = VariableFactory.enumerated("n", 0, base1, solver);
    IntVar D = VariableFactory.enumerated("d", 0, base1, solver);
    IntVar M = VariableFactory.enumerated("m", 0, base1, solver);
    IntVar O = VariableFactory.enumerated("o", 0, base1, solver);
    IntVar R = VariableFactory.enumerated("r", 0, base1, solver);
    IntVar Y = VariableFactory.enumerated("y", 0, base1, solver);

    x = new IntVar[] {S, E, N, D, M, O, R, Y};
 
    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    // coefficients
    int[] coefs = new int[13];
    for(int i = 0; i < 4; i++) {
      coefs[i] = (int)Math.pow(base, 4-i-1);
      coefs[4+i] = coefs[i];
      coefs[(4*2)+i] = -(int)Math.pow(base, 5-i-1);
      
    }
    coefs[12] = -1;       
    
    IntVar[] sendmoremoney = new IntVar[] { S, E, N, D, 
                                            M, O, R, E, 
                                            M, O, N, E, Y};
    solver.post(IntConstraintFactory.scalar(sendmoremoney, coefs, VariableFactory.fixed(0, solver)));

    solver.post(IntConstraintFactory.arithm(S,">", 0));
    solver.post(IntConstraintFactory.arithm(M,">", 0));
    
  }

  @Override
  public void createSolver() {
    solver = new Solver("SendMoreMoneyAnyBase");
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
      int num_solutions = 0;
      do {
        System.out.println("Base: " + base);
        for(int i = 0; i < x.length; i++) {
          System.out.println(x_s[i] + ": " + x[i].getValue());
        }
        System.out.println("\n");

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new SendMoreMoneyAnyBase().execute(args);

  }


}

