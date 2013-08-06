/**
 *
 * Magic Hexagon (CSPLib #23) in Choco3.
 *
 * From
 * http://www.cse.unsw.edu.au/~tw/csplib/prob/prob023/spec.html
 * """
 * A magic hexagon consists of the number 1 to 19 arranged in a hexagonal pattern:
 *
 *     A,B,C
 *    D,E,F,G
 *   H,I,J,K,L
 *    M,N,O,P
 *     Q,R,S
 * 
 * We have a constraint that all diagonals sum to 38. That is, 
 * A+B+C = D+E+F+G = ... = Q+R+S = 38, 
 * A+D+H = B+E+I+M = ... = L+P+S = 38, 
 * C+G+L = B+F+K+P = ... = H+M+Q = 38.
 * 
 * The problem can be generalized to other sizes. This is the diameter 
 * 5 problem. 
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

public class MagicHexagon extends AbstractProblem {
  
  int n = 19;


  IntVar[] x;


  @Override
  public void buildModel() {

    
    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);
    IntVar A = x[0];
    IntVar B = x[1];
    IntVar C = x[2];
    IntVar D = x[3];
    IntVar E = x[4];
    IntVar F = x[5];
    IntVar G = x[6];
    IntVar H = x[7];
    IntVar I = x[8];
    IntVar J = x[9];
    IntVar K = x[10];
    IntVar L = x[11];
    IntVar M = x[12];
    IntVar N = x[13];
    IntVar O = x[14];
    IntVar P = x[15];
    IntVar Q = x[16];
    IntVar R = x[17];
    IntVar S = x[18];


    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    IntVar[][] sums = new IntVar[][] { new IntVar[] {A,B,C},
                                       new IntVar[] {D,E,F,G},
                                       new IntVar[] {H,I,J,K,L},
                                       new IntVar[] {M,N,O,P},
                                       new IntVar[] {Q,R,S},
                                       new IntVar[] {A,D,H},
                                       new IntVar[] {B,E,I,M},
                                       new IntVar[] {C,F,J,N,Q},
                                       new IntVar[] {G,K,O,R},
                                       new IntVar[] {L,P,S},
                                       new IntVar[] {C,G,L},
                                       new IntVar[] {B,F,K,P},
                                       new IntVar[] {A,E,J,O,S},
                                       new IntVar[] {D,I,N,R},
                                       new IntVar[] {H,M,Q}};

    IntVar V38 = VariableFactory.fixed(38, solver);
    for(int i = 0; i < sums.length; i++) {
      solver.post(IntConstraintFactory.sum(sums[i], V38));
    }


    // Symmetry breaking
    solver.post(IntConstraintFactory.arithm(A, "<", C));
    solver.post(IntConstraintFactory.arithm(A, "<", H));
    solver.post(IntConstraintFactory.arithm(A, "<", L));
    solver.post(IntConstraintFactory.arithm(A, "<", Q));
    solver.post(IntConstraintFactory.arithm(A, "<", S));
    solver.post(IntConstraintFactory.arithm(C, "<", H));
    

  }

  @Override
  public void createSolver() {
    solver = new Solver("MagicHexagon");
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

        System.out.print("position: ");
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

    new MagicHexagon().execute(args);

  }

}

