package org.jcp.jsr331.hakan;

/**
 * xkcd Knapsack problem in Minizinc.
 *
 *  http://xkcd.com/287/
 *
 * Some amount (or none) of each dish should be ordered to 
 * give a total of exact 15.05
 *
 * Compare with the following models:
 * - Comet: http://www.hakank.org/comet/xkcd.co
 * - ECLiPSE: http://www.hakank.org/eclipse/xkcd.ecl
 * - Gecode: http://www.hakank.org/gecode/xkcd.cpp
 * - Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/xkcd.py
 * - MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn
 * - SICStus: http://www.hakank.org/sicstus/xkcd.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/xkcd.eprime
 * - Zinc: http://www.hakank.org/minizinc/xkcd.zinc
 *
 * Model created by Hakan Kjellerstrand, hakank@bonetmail.com
 * Also see http://www.hakank.org/jsr_331/
 *
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class xkcd {

    Var[] x;
    Problem p = ProblemFactory.newProblem("xkcd");

    // main
    public static void main(String[] args) {

        xkcd pp = new xkcd();
        pp.define();
        pp.solve();

    }
    

    // Problem definition    
    public void define() {

        int n = 6;
        int[] price = {215, 275, 335, 355, 420, 580};
        int total = 1505; // multiply by 100 to be able to use integers


        // variables
        x = p.variableArray("x", 0, 100, n);
        Var z = p.variable("z", 0, 10000);

        
        // constraints
        p.post(p.scalProd(price, x), "=", z);
        p.post(z, "=", total);
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
        
        //
        // tracing
        //
        // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
        // solver.traceExecution(true);

        //
        // solve
        //        
        int num_sols = 0;
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            System.out.println("z: " + s.getValue("z"));
            System.out.print("x: ");
            for(int i = 0; i < x.length; i++) {
                    System.out.print(s.getValue("x-"+i) + " ");
            }
            System.out.println();

            System.out.println();

            // s.log();
        }
        solver.logStats();
    }

}
