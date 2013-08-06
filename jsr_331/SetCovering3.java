package org.jcp.jsr331.hakan;

/**
  *
  * Set covering problem in JSR-331.
  *
  * Problem from
  * Katta G. Murty: 'Optimization Models for Decision Making',
  * page 302f
  * http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
  *
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetCovering3 {

  Problem p = ProblemFactory.newProblem("SetCovering3");

  int num_groups = 6;
  int num_senators = 10;

  // which group does a senator belong to?
  int[][] belongs = {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0},   // 1 southern
                     {0, 0, 0, 0, 0, 1, 1, 1, 1, 1},   // 2 northern
                     {0, 1, 1, 0, 0, 0, 0, 1, 1, 1},   // 3 liberals
                     {1, 0, 0, 0, 1, 1, 1, 0, 0, 0},   // 4 conservative
                     {0, 0, 1, 1, 1, 1, 1, 0, 1, 0},   // 5 democrats
                     {1, 1, 0, 0, 0, 0, 0, 1, 0, 1}};  // 6 republicans
  


  public static void main(String[] args) {
    SetCovering3 pp = new SetCovering3();
    pp.define();
    pp.solve();
  }
    

  // Problem definition    
  public void define() {
        
    //
    // variables
    // 
    Var[] x = p.variableArray("x", 0, 1, num_senators);
    Var z = p.sum(x);
    z.setName("z");
    p.add(z);


    //
    // constraints
    // 

    // ensure that each group is covered by at least
    // one senator
    for(int i = 0; i < num_groups; i++) {
      Var[] b = new Var[num_senators];
      for(int j = 0; j < num_senators; j++) {
        b[j] = x[j].multiply(belongs[i][j]);
      }
      p.post(b, ">=", 1);
    }


  }
    
    
  public void solve() {
    //
    // search
    //
    Solver solver = p.getSolver();
    SearchStrategy strategy = solver.getSearchStrategy();
        
    // strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
    // strategy.setVarSelectorType(VarSelectorType.MIN_VALUE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_VALUE);
    strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MIN_VALUE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_RANDOM);
    // strategy.setVarSelectorType(VarSelectorType.RANDOM);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MAX_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_WEIGHTED_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_WEIGHTED_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_IMPACT);
    // strategy.setVarSelectorType(VarSelectorType.MAX_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_REGRET);
        
        
        
        
    // strategy.setValueSelectorType(ValueSelectorType.IN_DOMAIN);
    strategy.setValueSelectorType(ValueSelectorType.MIN);
    // strategy.setValueSelectorType(ValueSelectorType.MAX);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_MAX_ALTERNATE);
    // strategy.setValueSelectorType(ValueSelectorType.MIDDLE);
    // strategy.setValueSelectorType(ValueSelectorType.MEDIAN);
    // strategy.setValueSelectorType(ValueSelectorType.RANDOM);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_IMPACT);
    // strategy.setValueSelectorType(ValueSelectorType.CUSTOM);
        
    // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
        
    //
    // solve
    //
    // solver.traceExecution(true);
        
    Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("z"));

    if (solution == null) {

      System.out.println("No solution");

    } else {
      
      System.out.println("z: " + solution.getValue("z") + " (senators)");
      System.out.print("x: ");
      for(int j = 0; j < num_senators; j++) {
        System.out.print(solution.getValue("x-"+j) + " ");
      }
      System.out.println();

      // More details
      for(int j = 0; j < num_senators; j++) {
        if (solution.getValue("x-"+j) == 1) {
          System.out.print("Senator " + (1 + j) +
                           " belongs to these groups: ");
          for(int i = 0; i < num_groups; i++) {
            if (belongs[i][j] == 1) {
              System.out.print((1 + i) + " ");
            }
          }
          System.out.println();
        }
      }

      System.out.println();

    }
    solver.logStats();
  }

}
