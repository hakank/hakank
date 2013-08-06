/**
 *
 * Five floors problem in Choco3
 *
 * From Alexey Radul & Gerald Jay Sussman:
 * "The Art of Propagator", page 34
 * """
 * Baker, Cooper, Fletcher, Miller, and Smith live on the first
 * five floors of this apartment house. Baker does not live on the
 * fifth floor. Cooper does not live on the first floor. Fletcher
 * does not live on either the fifth or the first floor. Miller lives
 * on a higher floor than does Cooper. Smith does not live on a
 * floor adjacent to Fletcher'. Fletcher does not live on a floor
 * adjacent to Cooper's.
 * """


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
import util.tools.StatisticUtils;

import java.util.*;
import java.util.Random;

public class FiveFloors extends AbstractProblem {

  int n = 5;

  IntVar[] x;

  @Override
  public void buildModel() {

    x = VariableFactory.boundedArray("x", n, 1, n, solver);
    IntVar Baker    = x[0];
    IntVar Cooper   = x[1];
    IntVar Fletcher = x[2];
    IntVar Miller   = x[3];
    IntVar Smith    = x[4];


    IntVar c1 = VariableFactory.fixed(1, solver);
    IntVar c5 = VariableFactory.fixed(5, solver);

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    //  Baker does not live on the fifth floor.
    solver.post(IntConstraintFactory.arithm(Baker, "!=", c5));

    //  Cooper does not live on the first floor. 
    solver.post(IntConstraintFactory.arithm(Cooper, "!=", c1));

    //  Fletcher does not live on either the fifth or the first floor. 
    solver.post(IntConstraintFactory.arithm(Fletcher, "!=", c5));
    solver.post(IntConstraintFactory.arithm(Fletcher, "!=", c1));

    //  Miller lives on a higher floor than does Cooper. 
    solver.post(IntConstraintFactory.arithm(Miller, ">", Cooper));

    //  Smith does not live on a floor adjacent to Fletcher'. 
    solver.post(IntConstraintFactory.distance(Smith, Fletcher, ">", c1));

    //  Fletcher does not live on a floor adjacent to Cooper's.
    solver.post(IntConstraintFactory.distance(Fletcher, Cooper, ">", c1));


  }

  @Override
  public void createSolver() {
    solver = new Solver("FiveFloors");
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

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.println();

        num_solutions++;

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

    new FiveFloors().execute(args);

  }
}

