/**
  *
  * Bus scheduling in Choco3.
  *
  *
  * Minimize number of buses in timeslots.
  * Problem from Taha "Introduction to Operations Research", page 58.
  * 
  * This is a slightly more general model than Taha's.
  *
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
import solver.constraints.propagators.extension.nary.IterTuplesTable;
import solver.constraints.propagators.extension.nary.LargeRelation;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class BusSchedule extends AbstractProblem {

  int time_slots = 6;
  int max_num;
  int [] demands = {8, 10, 7, 12, 4, 4};


  IntVar[] x;
  IntVar num_buses;


  @Override
  public void createSolver() {
    solver = new Solver("BusSchedule");
  }
  

  @Override
  public void buildModel() {    

    max_num = 0;
    for (int i : demands) {
      max_num += i;
    }

    x = VariableFactory.boundedArray("x", time_slots, 0, max_num, solver);    

    num_buses = VariableFactory.bounded("num_buses", 0, max_num*time_slots, solver);
    solver.post(IntConstraintFactory.sum(x, num_buses));

    // Meet the demands for this and the next time slot.
    for(int i = 0; i < time_slots - 1; i++) {
      solver.post(IntConstraintFactory.arithm(x[i],"+", x[i+1], ">=", demands[i]));
    }
    
    // The demand "around the clock"
    solver.post(IntConstraintFactory.arithm(x[time_slots-1],"+", x[0], ">=", demands[time_slots-1]));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }
  
  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, num_buses);
  }

  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("num_buses: " + num_buses.getValue());
        System.out.print("x: ");
        for (int i = 0; i < time_slots; i++) {
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

    new BusSchedule().execute(args);

  }


}
 
