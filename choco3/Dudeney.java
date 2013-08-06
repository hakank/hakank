/**
 *
 * Dudeney numbers in Choco3.
 *
 * From Pierre Schaus blog post
 * "Dudeney number"
 * http://cp-is-fun.blogspot.com/2010/09/test-python.html
 * """
 * I discovered yesterday Dudeney Numbers
 * A Dudeney Numbers is a positive integer that is a perfect cube such that the sum
 * of its decimal digits is equal to the cube root of the number. There are only six
 * Dudeney Numbers and those are very easy to find with CP.
 * I made my first experience with google cp solver so find these numbers (model below)
 * and must say that I found it very convenient to build CP models in python!
 * When you take a close look at the line:
 *     solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
 * It is difficult to argue that it is very far from dedicated
 * optimization languages!
 * """
 *
 * Also see: http://en.wikipedia.org/wiki/Dudeney_number
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
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Dudeney extends AbstractProblem {

  int n = 6;


  IntVar[] x;
  IntVar nb;
  IntVar s;

  //
  // Decomposition of product(IntVar[] v) 
  //   returns IntVar: the product of elements in array v
  //
  // Assumption: all element are >= 0.
  //
  public IntVar product(IntVar[] v) {
    int len = v.length;
    int max_val = 1; // maximum possible value
    int min_val = 1;
    for(int i = 0; i < len; i++) {
      max_val *= v[i].getUB();
      min_val *= v[i].getLB();
    }
    IntVar[] prod = VariableFactory.boundedArray("prod", len, min_val, max_val, solver);
    prod[0] = v[0];
    for(int i = 1; i < len; i++) {
      solver.post(IntConstraintFactory.times(prod[i-1], v[i], prod[i]));
    }

    return prod[len-1];

  }


  @Override
  public void buildModel() {
  
    x = VariableFactory.enumeratedArray("x", n, 0, 9, solver);
    nb = VariableFactory.bounded("nb", 1, (int)Math.pow(10, n), solver);
    s = VariableFactory.bounded("s", 1, 9*n+1, solver);

    // nb = s*s*s
    IntVar s3 = product(new IntVar[]{s,s,s});
    solver.post(IntConstraintFactory.arithm(nb, "=", s3));

    solver.post(IntConstraintFactory.sum(x, s));

    IntVar[] tmp = VariableFactory.boundedArray("tmp", n, 0, (int)Math.pow(10, n), solver);
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.times(x[i],
                                             VariableFactory.fixed((int)Math.pow(10,n-i-1), solver), 
                                             tmp[i]));
    }

    solver.post(IntConstraintFactory.sum(tmp, nb));


  }


  @Override
  public void createSolver() {
    solver = new Solver("Dudeney");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.maxReg_InDomainMin(x));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      ArrayList<Integer> sols = new ArrayList<Integer>();
      do {
        System.out.println("nb: " + nb.getValue());
        System.out.println("s: " + s.getValue());
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");         
        }
        System.out.println();

        sols.add(nb.getValue());

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      System.out.println("Dudeney numbers: " + sols);

      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new Dudeney().execute(args);

  }

}

