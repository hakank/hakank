/**
  *
  * Set covering problem in Choco3.
  *
  * Placing of firestations, from Winston "Operations Research", page 486
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class SetCovering extends AbstractProblem {

  int[][] distance;
  int min_distance;
  int num_cities;

  IntVar[] x;
  IntVar z;


  @Override
  public void createSolver() {
    solver = new Solver("SetCovering");
  }
  

  @Override
  public void buildModel() {    

    
    min_distance = 15;
    num_cities = 6;

    distance = new int[][] {{ 0,10,20,30,30,20},
                            {10, 0,25,35,20,10},
                            {20,25, 0,15,30,20},
                            {30,35,15, 0,15,25},
                            {30,20,30,15, 0,14},
                            {20,10,20,25,14, 0}};
    
    x = VariableFactory.enumeratedArray("x", num_cities, 0, 1, solver);
    z = VariableFactory.bounded("z", 0, 10000, solver);


    solver.post(IntConstraintFactory.sum(x, z));

    // ensure that all cities are covered
    for(int i = 0; i < num_cities; i++) {
      ArrayList<IntVar> b = new ArrayList<IntVar>();
      for(int j = 0; j < num_cities; j++) {
        if (distance[i][j] <= min_distance) {
          b.add(x[j]);
        }
      }
      IntVar ssum = VariableFactory.bounded("ssum", 0, num_cities, solver);
      solver.post(IntConstraintFactory.sum(b.toArray(new IntVar[1]), ssum));
      solver.post(IntConstraintFactory.arithm(ssum,">=", VariableFactory.fixed(1, solver)));

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
        for (int i = 0; i < num_cities; i++) {
            System.out.print(x[i].getValue() + " ");
        }

        System.out.println();
        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new SetCovering().execute(args);
  }


} // end class
 
