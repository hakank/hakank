/**
 *
 * Just forgotten puzzle (Enigma 1517) in Choco3.
 *
 * From http://www.f1compiler.com/samples/Enigma 201517.f1.html
 * """
 * Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.
 *
 * Joe was furious when he forgot one of his bank account numbers.
 * He remembered that it had all the digits 0 to 9 in some order,
 * so he tried the following four sets without success:
 *
 * 9 4 6 2 1 5 7 8 3 0
 * 8 6 0 4 3 9 1 2 5 7
 * 1 6 4 0 2 9 7 8 5 3
 * 6 8 2 4 3 1 9 0 7 5
 *
 * When Joe finally remembered his account number, he realised that
 * in each set just four of the digits were in their correct position
 * and that, if one knew that, it was possible to work out his
 * account number. What was it?
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
import solver.constraints.LogicalConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class JustForgotten extends AbstractProblem {


  int rows = 4;
  int cols = 10;

  // The four tries
  int[][] a = {{9,4,6,2,1,5,7,8,3,0},
               {8,6,0,4,3,9,1,2,5,7},
               {1,6,4,0,2,9,7,8,5,3},
               {6,8,2,4,3,1,9,0,7,5}};

  IntVar[] x;


  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedArray("x", cols, 0, 9, solver);

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    for(int r = 0; r < rows; r++) {
      BoolVar[] b = VariableFactory.boolArray("b", cols, solver);
      for(int c = 0; c < cols; c++) {
        /*
        solver.post(IntConstraintFactory.implies(b[c],
                                                 IntConstraintFactory.arithm(x[c], "=", a[r][c])));
        solver.post(IntConstraintFactory.implies(VariableFactory.not(b[c]),
                                                 IntConstraintFactory.arithm(x[c], "!=", a[r][c])));
        */
        solver.post(LogicalConstraintFactory.ifThen(b[c],
                                                    IntConstraintFactory.arithm(x[c], "=", a[r][c])));

      }
      
      solver.post(IntConstraintFactory.sum(b,VariableFactory.fixed(4, solver)));

    }



  }

  @Override
  public void createSolver() {
    solver = new Solver("JustForgotten");
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

        for(int i = 0; i < cols; i++) {
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

    new JustForgotten().execute(args);

  }

}

