/**
  *
  * Organizing a day problem in Choco3.
  *
  * Simple scheduling problem.
  *
  * Problem formulation from ECLiPSe:
  * Slides on (Finite Domain) Constraint Logic Programming, page 38f
  * http://eclipse-clp.org/reports/eclipse.ppt
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
import solver.constraints.LogicalConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class OrganizeDay extends AbstractProblem {

  int n = 4;

  int work = 0;
  int mail = 1;
  int shop = 2;
  int bank = 3;
  int[] tasks = {work, mail, shop, bank};
  int[] durations = {4,1,2,1};

  String[] tasks_s = {"Work", "Mail", "Shop", "Bank"};
  
  // task [i,0] must be finished before task [i,1]
  int[][] before_tasks = {
    {bank, shop},
    {mail, work}
  };
  
  // the valid times of the day
  int begin = 9;
  int end   = 17;


  IntVar[] begins;
  IntVar[] ends;


  //
  // No overlapping of tasks s1 and s2
  //
  public void NoOverlap(IntVar s1, int d1,
                               IntVar s2, int d2)
  {
    // (s1 + d1 <= s2) + (s2 + d2 <= s1) == 1
    BoolVar[] b = VariableFactory.boolArray("b", 2, solver);
    solver.post(LogicalConstraintFactory.ifThenElse(b[0],
                                                    IntConstraintFactory.arithm(VariableFactory.offset(s1,d1), "<=", s2),
                                                    IntConstraintFactory.arithm(VariableFactory.offset(s1,d1), ">", s2)));

    solver.post(LogicalConstraintFactory.ifThenElse(b[1],
                                                    IntConstraintFactory.arithm(VariableFactory.offset(s2,d2), "<=", s1),
                                                    IntConstraintFactory.arithm(VariableFactory.offset(s2,d2), ">", s1)));


    solver.post(IntConstraintFactory.sum(b, VariableFactory.fixed(1, solver)));
                
  }


  @Override
  public void createSolver() {
    solver = new Solver("OrganizeDay");
  }
  

  @Override
  public void buildModel() {    

    begins = VariableFactory.enumeratedArray("begins", n, begin, end, solver);
    ends = VariableFactory.enumeratedArray("ends", n, begin, end, solver);
    
    for(int t : tasks) {
      solver.post(IntConstraintFactory.arithm(VariableFactory.offset(begins[t], durations[t]), "=", ends[t]));
    }

    for(int i : tasks) {
      for(int j : tasks) {
        if (i < j) {
          NoOverlap(begins[i], durations[i],
                    begins[j], durations[j]);
        }
      }
    }
    
    // specific constraints
    for(int t = 0; t < before_tasks.length; t++) {
      solver.post(IntConstraintFactory.arithm(ends[before_tasks[t][0]], "<=", begins[before_tasks[t][1]]));
    }
    
    solver.post(IntConstraintFactory.arithm(begins[work], ">=", 11));
    
    
    
  }
  

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(begins, ends)));
  }
  
  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("Tasks: ");
        for (int i = 0; i < n; i++) {
          System.out.format("%s: %2d..%2d\n", tasks_s[i], begins[i].getValue(), ends[i].getValue());
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
    new OrganizeDay().execute(args);
  }


} // end class
 
