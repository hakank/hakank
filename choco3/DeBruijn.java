/**
  *
  * de Bruijn sequences in Choco3
  *
  * Both "normal" and "arbitrary" de Bruijn sequences.
  * 
  * This is a port from my MiniZinc model (via a Choco 2 model)
  *    http://www.hakank.org/minizinc/debruijn_binary.mzn
  *
  * and is explained somewhat in the Swedish blog post
  * "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  * http://www.hakank.org/webblogg/archives/001209.html
  *
  * Related programs:
  * - "Normal" de Bruijn sequences
  *  CGI program for calculating the sequences
  *  http://www.hakank.org/comb/debruijn.cgi
  *  http://www.hakank.org/comb/deBruijnApplet.html (as Java applet)
  *
  * - "Arbitrary" de Bruijn sequences
  *   Program "de Bruijn arbitrary sequences"
  *   http://www.hakank.org/comb/debruijn_arb.cgi
  *
  *   This (Swedish) blog post explaines the program:
  *   "de Bruijn-sekvenser av godtycklig längd"
  *   http://www.hakank.org/webblogg/archives/001114.html
  *
  * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
  * Also see http://www.hakank.org/choco3/
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
import solver.search.limits.FailLimit;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;


public class DeBruijn extends AbstractProblem {

  //
  // Note: The names is the same as the MiniZinc model (cited above) for
  // easier comparison.
  //

  // These parameters may be set by the user:
  //  - base: the base to use. Also known as k. 
  //  - n   : number of bits representing the numbers 
  //  - m   : length of the sequence, defaults to m = base^n
  //  -num_solutions: number of solutions to show, default all (0)


  @Option(name = "-base", usage = "base (default 2).", required = false)
  int base = 2;

  @Option(name = "-n", usage = "n (default 3).", required = false)
  int n = 3;

  @Option(name = "-m", usage = "m (default 2^3).", required = false)
  int m = 0;

  @Option(name = "-num_solutions", usage = "num_solutions (default 0=all).", required = false)
  int num_solutions = 0;

  @Option(name = "-no_print", usage = "don't print solutions (default false).", required = false)
  boolean no_print = false;


  IntVar[] x;        // the decimal numbers
  IntVar[][] binary; // the representation of numbers in x, in the choosen base
  IntVar[] bin_code; // the de Bruijn sequence (first number in binary)


  // integer power method
  static int pow( int x, int y) {
    int z = x; 
    for( int i = 1; i < y; i++ ) z *= x;
    return z;
  } // end pow


  @Override
  public void createSolver() {
    solver = new Solver("de Bruijn");
  }

  @Override
  public void buildModel() {

    int pow_base_n = pow(base,n); // base^n, the range of integers
    if (m > 0) {
      if (m > pow_base_n) {
        System.out.println("m must be <= base^n (" + m + ")");
        System.exit(1);
      }
    } else {
      m = pow_base_n;
    }

    System.out.println("Using base: " + base + " n: " + n + " m: " + m);

    // decimal representation, ranges from 0..base^n-1
    x = VariableFactory.boundedArray("x", m, 0, pow_base_n-1, solver);

        
    //
    // convert between decimal number in x[i] and "base-ary" numbers 
    // in binary[i][0..n-1].
    //
    // (This corresponds to the predicate toNum in the MiniZinc model)
    //

    // calculate the weights array
    int[] weights = new int[n];
    int w = 1;
    for(int i = 0; i < n; i++) {
      weights[n-i-1] = w;
      w *= base;            
    }

    // connect binary <-> x
    binary = new IntVar[m][n];
    for(int i = 0; i < m; i++) {
      binary[i] = VariableFactory.boundedArray("binary_" + i, n, 0, base-1, solver);
      solver.post(IntConstraintFactory.scalar(binary[i], weights, x[i]));
    }

    //
    // assert the the deBruijn property:  element i in binary starts
    // with the end of element i-1
    //
    for(int i = 1; i < m; i++) {
      for(int j = 1; j < n; j++) {
        solver.post(IntConstraintFactory.arithm(binary[i-1][j], "=", binary[i][j-1]));
      }
    }

    // ... "around the corner": last element is connected to the first
    for(int j = 1; j < n; j++) {
      solver.post(IntConstraintFactory.arithm(binary[m-1][j], "=", binary[0][j-1]));
    }


    //
    // This is the de Bruijn sequence, i.e.
    // the first element of of each row in binary[i]
    //
    bin_code = new IntVar[m];
    for(int i = 0; i < m; i++) {
      bin_code[i] = VariableFactory.bounded("bin_code_" + i, 0, base-1, solver);
      solver.post(IntConstraintFactory.arithm(bin_code[i], "=", binary[i][0]));
    }

        
    // All values in x should be different
    solver.post(IntConstraintFactory.alldifferent(x,"BC"));

    // Symmetry breaking: the minimum value in x should be the
    // first element.
    solver.post(IntConstraintFactory.minimum(x[0], x));


  } // end model


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

      int num_sols = 0;
      do {

        if (!no_print) {

          System.out.print("\ndecimal values: ");
          for (int i = 0; i < m; i++) {
            System.out.print(x[i].getValue() + " ");
          }
          
          System.out.print("\nde Bruijn sequence: ");
          for(int i = 0; i < m; i++) {
            System.out.print(bin_code[i].getValue() + " ");
          }
          
          System.out.println("\nbinary:");
          for(int i = 0; i < m; i++) {
            for(int j = 0; j < n; j++) {
              System.out.print(binary[i][j].getValue() + " ");
            }
            System.out.println(" : " + x[i].getValue());
          }
          
          System.out.println("\n");
          
        }

        num_sols++;
        if (num_solutions > 0 && num_sols >= num_solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);
            

      System.out.println("\nNumber of solutions: " + num_sols);

    } else {

      System.out.println("No solutions.");

    }// end if result


  }

  //
  // Running the program
  //  * java DeBruijn base n
  //  * java DeBruijn base n m
  //  * java DeBruijn base n m num_solutions
  //
  public static void main(String args[]) {

    new DeBruijn().execute(args);

  } // end main

} // end class

 