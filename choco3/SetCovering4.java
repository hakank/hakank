/**
  *
  * Set covering problem in Choco3.
  *    
  * Set partition and set covering problem from
  * Example from the Swedish book
  * Lundgren, Roennqvist, Vaebrand
  * "Optimeringslaera" (translation: "Optimization theory"),
  * page 408.
  *
  * This Choco3 model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my Choco page: http://www.hakank.org/choco/ 
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
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class SetCovering4 extends AbstractProblem {

  @Option(name = "-set_partition", usage = "Use set partition (default false, i.e. set_covering).", required = false)
  boolean set_partition = false;


  int[][] a; 
  int[] costs;
  int num_alternatives;
  int num_objects;

  IntVar[] x;
  IntVar z;


  @Override
  public void createSolver() {
    solver = new Solver("SetCovering4");
  }
  

  @Override
  public void buildModel() {    

    num_alternatives = 10;
    num_objects = 8;

    // costs for the alternatives
    costs = new int[] {19, 16, 18, 13, 15, 19, 15, 17, 16, 15};

    // the alternatives, and their objects
    a = new int[][] {
      // 1 2 3 4 5 6 7 8    the objects
        {1,0,0,0,0,1,0,0},  // alternative 1
        {0,1,0,0,0,1,0,1},  // alternative 2
        {1,0,0,1,0,0,1,0},  // alternative 3
        {0,1,1,0,1,0,0,0},  // alternative 4
        {0,1,0,0,1,0,0,0},  // alternative 5
        {0,1,1,0,0,0,0,0},  // alternative 6
        {0,1,1,1,0,0,0,0},  // alternative 7
        {0,0,0,1,1,0,0,1},  // alternative 8
        {0,0,1,0,0,1,0,1},  // alternative 9
        {1,0,0,0,0,1,1,0}}; // alternative 10


    x = VariableFactory.enumeratedArray("x", num_alternatives, 0, 1, solver);
    z = VariableFactory.bounded("z", 0, 1000, solver);

    // to be minimized
    solver.post(IntConstraintFactory.sum(x, z));

    for(int j = 0; j < num_objects; j++) {
      IntVar[] b = VariableFactory.boolArray("b", num_alternatives, solver);
      for(int i = 0; i < num_alternatives; i++) {
        solver.post(IntConstraintFactory.times(x[i], 
                                               VariableFactory.fixed(a[i][j],solver), 
                                               b[i]));

      }
      IntVar bsum = VariableFactory.bounded("bsum", 0, num_alternatives, solver);
      solver.post(IntConstraintFactory.sum(b, bsum));
      if (set_partition) {
        solver.post(IntConstraintFactory.arithm(bsum,">=", VariableFactory.fixed(1, solver)));
      } else {
        solver.post(IntConstraintFactory.arithm(bsum,"=", VariableFactory.fixed(1, solver)));
      }
    }


  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }
  
  @Override
  public void solve() {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("z: " + z.getValue());
        for (int i = 0; i < num_alternatives; i++) {
            System.out.print(x[i].getValue() + " ");
        }
        System.out.println();

        System.out.print("Selected alternatives: ");
        for (int i = 0; i < num_alternatives; i++) {
          if (x[i].getValue() == 1) {
            System.out.print(i + " ");
          }
        }
        System.out.println("\n");

        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new SetCovering4().execute(args);
  }


} // end class
 
