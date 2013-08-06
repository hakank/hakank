package org.jcp.jsr331.hakan;

/**
  *
  * Set covering problem (OPL) in JSR331.
  *
  * 
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

public class CoveringOpl {

  int num_workers = 32;
  int num_tasks = 15;

  Problem p = ProblemFactory.newProblem("CoveringOpl");

  public static void main(String[] args) {
    CoveringOpl pp = new CoveringOpl();
    pp.define();
    pp.solve();
  }
    

  // Problem definition    
  public void define() {

    //
    // data
    //

    // Which worker is qualified for each task.
    // Note: This is 1-based and will be made 0-base below.
    int[][] qualified =  {{ 1,  9, 19,  22,  25,  28,  31 },
                          { 2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 },
                          { 3, 10, 19, 24, 26, 30, 32 },
                          { 4, 21, 25, 28, 32 },
                          { 5, 11, 16, 22, 23, 27, 31 },
                          { 6, 20, 24, 26, 30, 32 },
                          { 7, 12, 17, 25, 30, 31 } ,
                          { 8, 17, 20, 22, 23  },
                          { 9, 13, 14,  26, 29, 30, 31 },
                          { 10, 21, 25, 31, 32 },
                          { 14, 15, 18, 23, 24, 27, 30, 32 },
                          { 18, 19, 22, 24, 26, 29, 31 },
                          { 11, 20, 25, 28, 30, 32 },
                          { 16, 19, 23, 31 },
                          { 9, 18, 26, 28, 31, 32 }};

    int[] cost = {1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3,
                  3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9};
      
        
    //
    // define variables
    //


    // hire this worker?
    Var[] hire = p.variableArray("hire", 0, 1, num_workers);
    Var total_cost = p.scalProd(cost, hire);
    total_cost.setName("total_cost");
    p.add(total_cost);

    //
    // Constraints
    //
    for(int j = 0; j < num_tasks; j++) {
      // Sum the cost for hiring the qualified workers
      // (also, make 0-base).
      int len = qualified[j].length;
      Var[] tmp = new Var[len];
      for(int c = 0; c < len; c++) {
        tmp[c] = hire[qualified[j][c] - 1];
      }

      p.post(tmp,">=",1);

    }


  }
    
    
  public void solve() {
    //
    // search
    //
    Solver solver = p.getSolver();
        
    SearchStrategy strategy = solver.getSearchStrategy();
        
    // strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
    strategy.setVarSelectorType(VarSelectorType.MIN_VALUE);
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
    strategy.setValueSelectorType(ValueSelectorType.MIN_IMPACT);
    // strategy.setValueSelectorType(ValueSelectorType.CUSTOM);
        
    // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
        
    //
    // solve
    //
    // solver.traceExecution(true);
        
    Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("total_cost"));

    if (solution == null) {
      p.log("No solution");
    } else {
      System.out.println("total_cost: " + solution.getValue("total_cost"));
      System.out.print("Hire: ");
      for(int i = 0; i < num_workers; i++) {
        if (solution.getValue("hire-"+i) == 1) {
          System.out.print(i + " ");
        }
      }
      System.out.println("\n");
      

      // solution.log();

    }
    solver.logStats();
  }

}
