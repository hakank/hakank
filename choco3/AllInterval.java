/**
 *
 * All interval problem in Choco3.
 *
 * CSPLib problem number 7
 * http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
 * """
 * Given the twelve standard pitch-classes (c, c%, d, ...), represented by 
 * numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
 * once and in which the musical intervals between neighbouring notes cover 
 * the full set of intervals from the minor second (1 semitone) to the major 
 * seventh (11 semitones). That is, for each of the intervals, there is a 
 * pair of neigbhouring pitch-classes in the series, between which this 
 * interval appears. The problem of finding such a series can be easily 
 * formulated as an instance of a more general arithmetic problem on Z_n, 
 * the set of integer residues modulo n. Given n in N, find a vector 
 * s = (s_1, ..., s_n), such that (i) s is a permutation of 
 * Z_n = {0,1,...,n-1}; and (ii) the interval vector 
 * v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
 * Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
 * called an all-interval series of size n; the problem of finding such 
 * a series is the all-interval series problem of size n. We may also be 
 * interested in finding all possible series of a given size. 
 * """
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
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class AllInterval extends AbstractProblem {

  @Option(name = "-n", usage = "n (size of problem, default 8).", required = false)
  int n = 8;

  @Option(name = "-solutions", usage = "number of solutions (default 0, all).", required = false)
  int solutions = 0;

  IntVar[] x;
  IntVar[] diffs;


  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedArray("x", n, 0, n-1, solver);
    diffs = VariableFactory.enumeratedArray("diffs", n-1, 1, n-1, solver);

    for (int k = 0; k < n-1; k++) {
      solver.post(IntConstraintFactory.distance(x[k], x[k+1], "=", diffs[k]));
    }

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));
    solver.post(IntConstraintFactory.alldifferent(diffs, "BC"));

    // Symmetry breaking
    solver.post(IntConstraintFactory.arithm(x[0], "<", x[n-1]));
    solver.post(IntConstraintFactory.arithm(diffs[0], "<", diffs[1]));

  }

  @Override
  public void createSolver() {
    solver = new Solver("AllInterval");
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
      do {
        System.out.print("\nx:");
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.print("diffs:");

        for(int i = 0; i < n-1; i++) {
          System.out.print(diffs[i].getValue() + " ");
        }
        System.out.println();


        num_solutions++;
        
        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);


      System.out.println("\nIt was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }

  }


  public static void main(String args[]) {

    new AllInterval().execute(args);

  }


}

