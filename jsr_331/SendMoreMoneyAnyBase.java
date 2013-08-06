package org.jcp.jsr331.hakan;

/**
 *
 * SEND+MORE=MONEY in any base in JSR-331.
 *
 * Alphametic problem SEND+MORE=MONEY in any base.
 *
 * Examples:
 * Base 10 has one solution:
 *    [9, 5, 6, 7, 1, 0, 8, 2]
 * Base 11 has three soltutions:
 *    [10, 5, 6, 8, 1, 0, 9, 2]
 *    [10, 6, 7, 8, 1, 0, 9, 3]
 *    [10, 7, 8, 6, 1, 0, 9, 2]
 * 
 *
 * Note: If the argument to the program is 0 (zero), then all the solutions
 *       for base 10 to 20 is shown.
 * 
 * The number of solutions for base 10 to 30 is:
 *
 *   1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210,231,
 *
 * which is the triangular number sequence:
 * http://www.research.att.com/~njas/sequences/?q=1+3+6+10+15+21+28+36+45+55+66+&language=english&go=Search
 *
 *  I blogged about this relation in
 *  "Some other Gecode/R models, mostly recreational mathematics"
 *  http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html
 *
 * Also, compare with the following models:
 * - Comet   : http://www.hakank.org/comet/send_more_money_any_base.co
 * - ECLiPSE : http://www.hakank.org/eclipse/send_more_money_any_base.ecl
 * - Essence : http://www.hakank.org/tailor/send_more_money_any_base.eprime
 * - Gecode  : http://www.hakank.org/gecode/send_more_money_any_base.cpp
 * - Gecode/R: http://www.hakank.org/gecode_r/send_more_money_any_base.rb
 * - Google CP Solver: http://hakank.org/google_or_tools/send_more_money_any_base.py
 * - MiniZinc: http://www.hakank.org/minizinc/send_more_money_any_base.mzn
 * - SICStus: http://www.hakank.org/sicstus/send_more_money_any_base.pl
 * - Zinc: http://www.hakank.org/minizinc/send_more_money_any_base.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SendMoreMoneyAnyBase  {

    int base = 10;
    Problem p = ProblemFactory.newProblem("SendMoreMoneyAnyBase");

    // main
    public static void main(String[] args) {

        int base_in = 10;

        if (args.length >= 1) {
            base_in = Integer.parseInt(args[0]);
        }

        if (base_in == 0) {
            System.out.println("Testing all bases from 10 to 30:");
            for(int n = 10; n <= 30; n++) {
                System.out.println("\nbase: " + n + "\n");
                SendMoreMoneyAnyBase p = new SendMoreMoneyAnyBase();
                p.define(n);
                p.solve();
            }
        } else {

            System.out.println("\nbase: " + base_in + "\n");
            SendMoreMoneyAnyBase smm = new SendMoreMoneyAnyBase();
            smm.define(base_in);
            smm.solve();
        }

    }


    // Problem definition    
    public void define(int base_in) {

        base = base_in;
        int base_1 = base - 1;

        Var S = p.variable("S", 1, base_1);
        Var E = p.variable("E", 0, base_1);
        Var N = p.variable("N", 0, base_1);
        Var D = p.variable("D", 0, base_1);
        Var M = p.variable("M", 1, base_1);
        Var O = p.variable("O", 0, base_1);
        Var R = p.variable("R", 0, base_1);
        Var Y = p.variable("Y", 0, base_1);
        
        Var[] vars = { S, E, N, D, M, O, R, Y };

        /*
        // coefficients for base 10
        int coef[] = { 1000, 100, 10, 1, 
                       1000, 100, 10, 1, 
                       -10000, -1000, -100, -10, -1 };
        */
        int[] coef = new int[13];
        for(int i = 0; i < 4; i++) {
            coef[i] = (int)Math.pow(base, 4-i-1);
            coef[4+i] = coef[i];
            coef[(4*2)+i] = -(int)Math.pow(base, 5-i-1);

        }
        coef[12] = -1;       

        Var[] sendmoremoney = new Var[] { S, E, N, D, 
                                          M, O, R, E, 
                                          M, O, N, E, Y};

        try {
            p.post(coef, sendmoremoney, "=", 0);
            p.postAllDifferent(vars);
        } catch(Exception e) {
            p.log("Error: " + e);
            System.exit(-1);
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
        strategy.setValueSelectorType(ValueSelectorType.MIN_MAX_ALTERNATE);
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
        System.out.println();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            //s.log();
            
            System.out.print(s.getValue("S")+" "+s.getValue("E")+" "+s.getValue("N")+" "+s.getValue("D")+" ");
            System.out.println(s.getValue("M")+" "+s.getValue("O")+" "+s.getValue("R")+" "+s.getValue("Y"));

        }

        System.out.println("\nIt was " + num_sols + " solutions.\n");

        solver.logStats();
    }

}
