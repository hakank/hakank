/**
 *
 * Crew allocation problem in Choco3.
 *
 * From Gecode example crew
 * examples/crew.cc
 * """
 * Example: Airline crew allocation
 *
 * Assign 20 flight attendants to 10 flights. Each flight needs a certain
 * number of cabin crew, and they have to speak certain languages.
 * Every cabin crew member has two flights off after an attended flight.
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
import solver.constraints.LogicalConstraintFactory;
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

public class Crew extends AbstractProblem {
  
  @Option(name = "-solutions", usage = "Number solutions to show (default 1).", required = false)
  int solutions = 1;

  @Option(name = "-extra", usage = "Add extra constraint (default false).", required = false)
  boolean extra = false;

  @Option(name = "-opt", usage = "Optimize number of people working (default false).", required = false)
  boolean opt = false;
  

  String[] names = {"Tom",
                    "David",
                    "Jeremy",
                    "Ron",
                    "Joe",
                    "Bill",
                    "Fred",
                    "Bob",
                    "Mario",
                    "Ed",
                    "Carol",
                    "Janet",
                    "Tracy",
                    "Marilyn",
                    "Carolyn",
                    "Cathy",
                    "Inez",
                    "Jean",
                    "Heather",
                    "Juliet"};
  
  int num_persons = names.length;

  
  //
  // Attributes of the crew
  //
  int[][] attributes = {
    // steward, hostess, french, spanish, german
    {1,0,0,0,1},   // Tom     = 0
    {1,0,0,0,0},   // David   = 1
    {1,0,0,0,1},   // Jeremy  = 2
    {1,0,0,0,0},   // Ron     = 3
    {1,0,0,1,0},   // Joe     = 4
    {1,0,1,1,0},   // Bill    = 5
    {1,0,0,1,0},   // Fred    = 6
    {1,0,0,0,0},   // Bob     = 7
    {1,0,0,1,1},   // Mario   = 8
    {1,0,0,0,0},   // Ed      = 9
    {0,1,0,0,0},   // Carol   = 10
    {0,1,0,0,0},   // Janet   = 11
    {0,1,0,0,0},   // Tracy   = 12
    {0,1,0,1,1},   // Marilyn = 13
    {0,1,0,0,0},   // Carolyn = 14
    {0,1,0,0,0},   // Cathy   = 15
    {0,1,1,1,1},   // Inez    = 16
    {0,1,1,0,0},   // Jean    = 17
    {0,1,0,1,1},   // Heather = 18
    {0,1,1,0,0}    // Juliet  = 19
  };
  
  
  //
  // Required number of crew members.
  //
  // The columns are in the following order:
  // staff     : Overall number of cabin crew needed
  // stewards  : How many stewards are required
  // hostesses : How many hostesses are required
  // french    : How many French speaking employees are required
  // spanish   : How many Spanish speaking employees are required
  // german    : How many German speaking employees are required
  //
  int[][] required_crew = {
    {4,1,1,1,1,1}, // Flight 1
    {5,1,1,1,1,1}, // Flight 2
    {5,1,1,1,1,1}, // ..
    {6,2,2,1,1,1},
    {7,3,3,1,1,1},
    {4,1,1,1,1,1},
    {5,1,1,1,1,1},
    {6,1,1,1,1,1},
    {6,2,2,1,1,1}, // ...
    {7,3,3,1,1,1}  // Flight 10
  };

  int num_flights = required_crew.length;


  IntVar[][] crew;
  IntVar num_working;

  @Override
  public void buildModel() {

    crew = VariableFactory.enumeratedMatrix("crew", num_flights, num_persons, 0, 1, solver);
    num_working = VariableFactory.bounded("num_working", 0, num_persons, solver);

    // number of working persons
    BoolVar[] nw = VariableFactory.boolArray("nw", num_persons, solver);
    for(int p = 0; p < num_persons; p++) {
      IntVar[] tmp = new IntVar[num_flights];
      for(int f = 0; f < num_flights; f++) {
        tmp[f] = crew[f][p];
      }
      IntVar tmpSum = VariableFactory.enumerated("tmpSum", 0, num_flights, solver);
      solver.post(IntConstraintFactory.sum(tmp, tmpSum));
      solver.post(LogicalConstraintFactory.ifThenElse(nw[p],
                                                      IntConstraintFactory.arithm(tmpSum, ">", 0),
                                                      IntConstraintFactory.arithm(tmpSum, "=", 0)));


    }
    solver.post(IntConstraintFactory.sum(nw, num_working));

    for(int f = 0; f < num_flights; f++) {
      // size of crew
      IntVar[] tmp = new IntVar[num_persons];
      for(int p = 0; p < num_persons; p++) {
        tmp[p] = crew[f][p];
      }
      solver.post(IntConstraintFactory.sum(tmp,VariableFactory.fixed(required_crew[f][0], solver)));

      // attributes and requirements
      for(int a = 0; a < 5; a++) {
        IntVar[] tmp2 = VariableFactory.enumeratedArray("tmp2_"+a,num_persons, 0, num_flights,solver);
        for(int p = 0; p < num_persons; p++) {
          solver.post(IntConstraintFactory.times(crew[f][p], 
                                                 VariableFactory.fixed(attributes[p][a], solver),
                                                 tmp2[p]));
        }
        IntVar tmp2Sum = VariableFactory.bounded("tmp2Sum_"+a, 0, num_persons, solver);
        solver.post(IntConstraintFactory.sum(tmp2, tmp2Sum));
        solver.post(IntConstraintFactory.arithm(tmp2Sum, 
                                                ">=", 
                                                VariableFactory.fixed(required_crew[f][a+1], solver)));
      }
    }

    // after a flight, break for at least two flights
    for(int f = 0; f < num_flights - 2; f++) {
      for(int i = 0; i < num_persons; i++) {
        // crew[f,i] + crew[f+1,i] + crew[f+2,i] <= 1
        IntVar csum = VariableFactory.enumerated("csum_"+f+"_"+i, 0, 3, solver);
        solver.post(IntConstraintFactory.sum(new IntVar[] {crew[f][i],crew[f+1][i],crew[f+2][i]},
                                             csum));
        solver.post(IntConstraintFactory.arithm(csum,"<=",1));
      }
    }

    // extra contraint: all must work at least two of the flights
    if (extra) {
      for(int p = 0; p < num_persons; p++) {
        IntVar[] tmp = new IntVar[num_flights];
        for(int f = 0; f < num_flights; f++) {
          tmp[f] = crew[f][p];
        }
        IntVar extraSum = VariableFactory.bounded("extraSum_"+p, 0, num_flights, solver);
        solver.post(IntConstraintFactory.sum(tmp, extraSum));
        solver.post(IntConstraintFactory.arithm(extraSum,">=", 2));
      }
    }

  }


  @Override
  public void createSolver() {
    solver = new Solver("Crew");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.maxReg_InDomainMin(ArrayUtils.flatten(crew))); 
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(crew))); 
    // solver.set(IntStrategyFactory.inputOrder_InDomainMin(ArrayUtils.flatten(crew))); 
  }

  @Override
  public void solve() {
    if (opt) {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, num_working);
    } else {
      solver.findSolution();
    }
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("num_working: " + num_working.getValue());
        for(int f = 0; f < num_flights; f++) {
          for(int p = 0; p < num_persons; p++) {
            System.out.format("%d ", crew[f][p].getValue());
          }
          System.out.println();
        }
        System.out.println();
        
        
        System.out.println("\nFlights: ");
        for(int f = 0; f < num_flights; f++) {
          System.out.format("Flight %d: ", f);
          for(int p = 0; p < num_persons; p++) {
            if (crew[f][p].getValue() == 1) {
              System.out.print(names[p] + " ");
            }
          }
          System.out.println();
        }

        System.out.println("\nCrew:");
        for(int p = 0; p < num_persons; p++) {
          System.out.format("%-7s: ", names[p]);
          for(int f = 0; f < num_flights; f++) {
            if (crew[f][p].getValue() == 1) {
              System.out.print(f + " ");
            }
          }
          System.out.println();
        }
        
        System.out.println();
      
      

        num_solutions++;

        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new Crew().execute(args);

  }

}

