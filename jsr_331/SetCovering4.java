package org.jcp.jsr331.hakan;

/**
  *
  * Set partition/set covering problem in JSR-331.
  *
  * Set partition and set covering problem from
  * the Swedish book
  * Lundgren, Roennqvist, Vaebrand
  * 'Optimeringslaera' (translation: 'Optimization theory'),
  * page 408.
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetCovering4 {

  Problem p = ProblemFactory.newProblem("SetCovering4");
  int num_alternatives = 10;
  int num_objects = 8;

  public static void main(String[] args) {

    SetCovering4 pp = new SetCovering4();
    pp.define(0);
    pp.solve();

    pp = new SetCovering4();
    pp.define(1);
    pp.solve();


  }
    

  // Problem definition    
  public void define(int set_partition) {

    if (set_partition == 1) {
      System.out.println("\nSolving set partition");
    } else {
      System.out.println("\nSolving set covering");
    }

    //
    // data
    // 
    // costs for the alternatives
    int[] costs = {19, 16, 18, 13, 15, 19, 15, 17, 16, 15};

    // the alternatives, and their objects
    int[][] a = {
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



    //
    // variables
    // 
    Var[] x = p.variableArray("x", 0, 1, num_alternatives);
    Var z = p.scalProd(costs, x);
    z.setName("z");
    p.add(z);


    //
    // constraints
    // 
    for(int j = 0; j < num_objects; j++) {
      Var[] b = new Var[num_alternatives];
      for(int i = 0; i < num_alternatives; i++) {
        b[i] = x[i].multiply(a[i][j]);
      }

      if (set_partition == 1) {
        p.post(b, "=", 1);
      } else {
        p.post(b, ">=", 1);
      }
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
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN);
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
    // strategy.setValueSelectorType(ValueSelectorType.MIN);
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

      System.out.println("z: " + solution.getValue("z"));
      System.out.print("Selected alternatives: ");
      for(int i = 0; i < num_alternatives; i++) {
        if (solution.getValue("x-"+i) == 1) {
          System.out.print((1 + i) + " ");
        }
      }

      System.out.println();

    }
    solver.logStats();
  }

}
