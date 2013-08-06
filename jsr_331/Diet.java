package org.jcp.jsr331.hakan;

/**
 *
 * Diet problem in JSR331.
 * 
 * Problem from http://www.mcs.vuw.ac.nz/courses/OPRE251/2006T1/Labs/lab09.pdf
 * """
 *  My diet requires that all the food I eat come from one of the four .basic 
 *  food groups. (chocolate cake, ice cream, soft drink, and cheesecake). 
 *  Each (large) slice of chocolate cake costs 50c, 
 *  each scoop of chocolate ice cream costs 20c, 
 *  each bottle of cola costs 30c, 
 *  and each piece of pineapple cheesecake costs 80c. 
 *
 *  Each day, I must ingest at least 500 calories, 
 *  6 oz of chocolate, 
 *  10 oz of sugar, 
 *  and 8 oz of fat.
 *  The nutritional content per unit of each food is shown in the table below. 
 * 
 *  Formulate a linear programming model that can be used to satisfy my daily 
 *  nutritional requirement at minimum cost.

 *  Type of                        Calories   Chocolate    Sugar    Fat
 *  Food                                      (ounces)     (ounces) (ounces)
 *  Chocolate Cake (1 slice)       400           3            2      2
 *  Chocolate ice cream (1 scoop)  200           2            2      4
 *  Cola (1 bottle)                150           0            4      1
 *  Pineapple cheesecake (1 piece) 500           0            4      5
 *
 * """  
 *
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/Diet.java
 * - Comet: http://www.hakank.org/comet/diet.co
 * - ECLiPSE: http://www.hakank.org/eclipse/diet.ecl
 * - Gecode/R: http://www.hakank.org/gecode_r/diet.rb
 * - Gecode: http://www.hakank.org/gecode/diet.cpp
 * - Google CP Solver: http://www.hakank.org/google_or_tools/diet1.py
 * - JaCoP: http://www.hakank.org/JaCoP/Diet.java
 * - MiniZinc: http://www.hakank.org/minizinc/diet1.mzn
 * - SICStus: http://www.hakank.org/sicstus/diet1.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/diet1.eprime
 * - Zinc: http://www.hakank.org/minizinc/diet1.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * See also http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

public class Diet {

    Var[] x;

    int n; // number of ingredients
    int m; // number of food types

    String[] food = {"Chocolate Cake", "Chocolate ice cream", "Cola", "Pineapple cheesecake"};
    String[] ingredients = {"Calories", "Chocolate", "Sugar", "Fat"};

    Problem p = ProblemFactory.newProblem("Diet");

    public static void main(String[] args) {
        Diet diet = new Diet();
        diet.define();
        diet.solve();
    }
    

    // Problem definition    
    public void define() {
        
        n = 4; // number of ingredients
        m = 4; // number of food types

        int[] maxVals = {200000, 2000000, 2000000, 200000};
        int[] price   = {50, 20, 30, 80}; // in cents
        int[] limits  = {500, 6, 10, 8};  // minimum required for a diet

                    // Food: 0   1     2    3
        int[][] matrix = {{400, 200, 150, 500},  // calories
                          {  3,   2,   0,   0},  // chocolate
                          {  2,   2,   4,   4},  // sugar
                          {  2,   4,   1,   5}}; // fat


        // define variables
        x = p.variableArray("x", 0, 10, m);

        Var[] sums = p.variableArray("sums", 0, 10000, n);

        Var cost = p.variable("cost", 0, 100000);

        // define and post constraints
        try {
            for(int i = 0; i < n; i++) {
                p.post(p.scalProd(matrix[i], x), "=", sums[i]); 
                p.post(sums[i], ">=", limits[i]);
            }
            
            p.post(p.scalProd(price, x), "=", cost);
            p.log(p.getVars());
        } catch (Exception e) {
            p.log("Error posting constraints: " + e);
            System.exit(-1);
        }
    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        
        SearchStrategy strategy = solver.getSearchStrategy();
        //strategy.setVars(x);
        
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
        
        Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("cost"));
        if (solution == null) {
            p.log("No solution");
        } else {
            solution.log();
            
            System.out.println("\nCost: " + solution.getValue("cost"));
            for(int i = 0; i < m; i++) {
                System.out.println(food[i] + ": " + solution.getValue("x-"+i));
            }
            System.out.println();

        }
        solver.logStats();
    }

}
