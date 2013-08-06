/**
 *
 * Costas array in Choco3.
 *
 * From http://mathworld.wolfram.com/CostasArray.html:
 * """
 * An order-n Costas array is a permutation on {1,...,n} such
 * that the distances in each row of the triangular difference
 * table are distinct. For example, the permutation {1,3,4,2,5}
 * has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
 * and {4}. Since each row contains no duplications, the permutation
 * is therefore a Costas array.
 * """
 * 
 * Also see
 * http://en.wikipedia.org/wiki/Costas_array
 *
 * Note: This model (or rather my MiniZinc version via my or-tools/C# model) 
 * is based on Barry O'Sullivan's  MiniZinc model 
 * in the G12 repository:
 *      http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn
 *
 * Here are the two rather simple differences 
 * (marked by "hakank" below)
 *  1) no symmetry breaking on the order of the Costas array
 *  2) fixes the lower triangular matrix in the difference
 *     matrix to -n+1
 * 
 * Since there is no symmetry breaking of the order of the Costas 
 * array it gives all the solutions for a specific length of 
 * the array, e.g. those listed in 
 *     http://mathworld.wolfram.com/CostasArray.html
 * 
 * 1	1	(1)
 * 2	2	(1, 2), (2,1)
 * 3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
 * 4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
 *               (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
 *               (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
 * ....
 * 
 * See http://www.research.att.com/~njas/sequences/A008404
 * for the number of solutions for n=1..
 * 1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
 * 17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
 * 872, 200, 88, 56, 204,...
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

import java.util.*;

public class CostasArray extends AbstractProblem {

  @Option(name = "-n", usage = "Size of the array (default 6).", required = false)
  int n = 6;

  IntVar[] costas;
  IntVar[][] differences;



  @Override
  public void buildModel() {

    costas = VariableFactory.enumeratedArray("costas", n, 1, n, solver);
    differences = VariableFactory.enumeratedMatrix("differences", n, n, -n+1, n-1, solver);

    IntVar zero = VariableFactory.fixed(0, solver);

    // Fix the values in the lower triangle in the
    // difference matrix to -n+1. This removes variants
    // of the difference matrix for the the same Costas array.
    IntVar n1 = VariableFactory.fixed(-n+1, solver);
    for(int i = 0; i < n; i++) {
      for(int j = 0; j <= i; j++ ) {
        solver.post(IntConstraintFactory.arithm(differences[i][j], "=", n1));
      }
    }

    // hakank: All the following constraints are from
    // Barry O'Sullivans's original MiniZinc model; the quoted comments
    // are Barry's as well.
    //
    solver.post(IntConstraintFactory.alldifferent(costas, "BC"));


    // "How do the positions in the Costas array relate
    //  to the elements of the distance triangle."
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (i < j) {
          solver.post(IntConstraintFactory.
                      sum(
                          new IntVar[] {differences[i][j], 
                                        VariableFactory.minus(costas[j]),
                                        costas[j-i-1]}, 
                          zero));
        }
      }
    }
    

    // "All entries in a particular row of the difference
    //  triangle must be distint."
    for(int i = 0; i < n-2; i++) {
      ArrayList<IntVar> tmp = new ArrayList<IntVar>();
      for(int j = 0; j < n; j++) {
        if (j > i) {
          tmp.add(differences[i][j]);
        }
      }
      solver.post(IntConstraintFactory.alldifferent(tmp.toArray(new IntVar[1]), "BC"));
        
    }
    
    //
    // "All the following are redundant - only here to speed up search."
    //

    // "We can never place a 'token' in the same row as any other."
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (i < j) {
          solver.post(IntConstraintFactory.arithm(differences[i][j], "!=", zero));
          solver.post(IntConstraintFactory.arithm(differences[i][j], "!=", zero));
        }
      }
    }

    for(int k = 2; k < n; k++) {
      for(int l = 2; l < n; l++) {
        if (k < l) {
          solver.post(IntConstraintFactory.
                      sum(
                          new IntVar[] { differences[k-2][l-1],
                                         differences[k][l],
                                         VariableFactory.minus(differences[k-1][l-1]),
                                         VariableFactory.minus(differences[k-1][l]) },
                          zero));
        }
      }
    }


  }


  @Override
  public void createSolver() {
    solver = new Solver("CostasArray");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.firstFail_InDomainMin(costas)); 
    // solver.set(IntStrategyFactory.maxReg_InDomainMin(costas)); 
    solver.set(IntStrategyFactory.inputOrder_InDomainMax(costas)); 
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
        System.out.print("costas   : ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", costas[i].getValue());
        }
        System.out.println("\ndifferences: ");
        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            int v = differences[i][j].getValue();
            if (v == -n+1) {
              System.out.print("   ");
            } else {
              System.out.format("%2d ", v);
            }
          }
          System.out.println();
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

    new CostasArray().execute(args);

  }

}

