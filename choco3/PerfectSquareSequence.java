/**
 *
 * Perfect square sequence in Choco3
 *
 * From 'Fun with num3ers'
 * "Sequence"
 * http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
 * """
 * If we take the numbers from 1 to 15 
 *    (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
 * and rearrange them in such an order that any two consecutive 
 * numbers in the sequence add up to a perfect square, we get,
 *  
 *  8     1     15     10     6     3     13     12      4      5     11     14        2      7      9
 *      9    16    25     16     9     16     25     16     9     16     25     16       9     16
 *
 *  
 * I ask the readers the following:
 *  
 * Can you take the numbers from 1 to 25 to produce such an arrangement?
 * How about the numbers from 1 to 100?
 * """
 *
 * Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910
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


public class PerfectSquareSequence extends AbstractProblem {

  @Option(name = "-n", usage = "Size of sequence (default 15).", required = false)
  int n = 15;

  @Option(name = "-solutions", usage = "Number of solutions to show (default 1).", required = false)
  int solutions = 1;


  IntVar[] x;

  @Override
  public void buildModel() {

    int squares[] = new int[n-1];
    for(int i = 1; i < n; i++) {
      squares[i-1] = i*i;
    }

    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    for(int i = 1; i < n; i++) {
      IntVar s = VariableFactory.bounded("s_"+i, 0, n*n, solver);
      solver.post(IntConstraintFactory.sum(new IntVar[]{x[i-1],x[i]},s));
      solver.post(IntConstraintFactory.member(s,squares));
    }

    // symmetry breaking
    solver.post(IntConstraintFactory.arithm(x[0], "<", x[n-1]));



  }

  @Override
  public void createSolver() {
    solver = new Solver("PerfectSquareSequence");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    // solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
    solver.findSolution();

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.println();

        num_solutions++;

        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);

      System.out.println("It was " + num_solutions + " solutions.");

    } else {

      System.out.println("Problem is not feasible.");

    }

  }


  //
  // main
  //
  public static void main(String args[]) {

    new PerfectSquareSequence().execute(args);

  }
}

