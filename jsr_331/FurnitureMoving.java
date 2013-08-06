package org.jcp.jsr331.hakan;

/**
 *
 * Furniture Moving problem in choco v2.
 *
 * Problem from Marriott & Stuckey: 'Programming with constraints', page  112f
 *
 * Feature: testing cumulative.
 *
 * Note: JSR-331 don't support cumulative as of writing, so this model
 *       includes a decomposition of this global constraint.       
 *
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/FurnitureMoving.java
 * - Comet: http://www.hakank.org/comet/furniture_moving.co
 * - ECLiPSE: http://www.hakank.org/eclipse/furniture_moving.ecl
 * - Gecode: http://www.hakank.org/gecode/furniture_moving.cpp
 * - Google CP Solver: http://www.hakank.org/google_or_tools/furniture_moving.py
 * - JaCoP: http://www.hakank.org/JaCoP/FurnitureMoving.java
 * - MiniZinc: http://www.hakank.org/minizinc/furniture_moving.mzn
 * - SICStus: http://www.hakank.org/sicstus/furniture_moving.pl
 * - Zinc: http://www.hakank.org/minizinc/furniture_moving.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class FurnitureMoving {

    Problem p = ProblemFactory.newProblem("FurnitureMoving");

    /**
     *
     * Experimental decomposition of cumulative.
     *
     * Inspired by the MiniZinc implementation:
     * http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=g12:zinc:lib:minizinc:std:cumulative.mzn&s[]=cumulative
     * The MiniZinc decomposition is discussed in the paper:
     * A. Schutt, T. Feydy, P.J. Stuckey, and M. G. Wallace.
     * 'Why cumulative decomposition is not as bad as it sounds.'
     * Download:
     * http://www.cs.mu.oz.au/%7Epjs/rcpsp/papers/cp09-cu.pdf
     * http://www.cs.mu.oz.au/%7Epjs/rcpsp/cumu_lazyfd.pdf
     *
     * Parameters:
     *   s: start_times    assumption: array of IntVar
     *   d: durations      assumption: array of int
     *   r: resources      assumption: array of int
     *   b: resource limit assumption: IntVar or int
     */
    public void my_cumulative(Var[] s, int[] d, int[] r, Var b) {
        int n = s.length;

        int times_min = 999999;
        int times_max = 0;
        for(int i = 0; i < n; i++) {
            int s_min = s[i].getMin();
            int s_max = s[i].getMax();
            
            if (s_max > times_max) {
                times_max = s_max;
            }
            if (s_min < times_min) {
                times_min = s_min;
            }
        }

        int d_max = 0;
        for(int i = 0; i < n; i++) {
            if (d[i] > d_max) {
                d_max = d[i];
            }
        }        
        times_max = times_max + d_max;
        
        for(int t = times_min; t <= times_max; t++) {
            ArrayList<Var> bb = new ArrayList<Var>();
            for(int i = 0; i < n; i++) {
                Var c1 = p.linear(s[i], "<=", t).asBool();
                Var c2 = p.linear(s[i].plus(d[i]), ">", t).asBool();
                bb.add(c1.multiply(c2).multiply(r[i]));
            }
            p.post(p.sum(bb.toArray(new Var[1])), "<=", b);
        }

        // sanity check: ensure that b < sum(r)
        int sum_r = 0;
        for(int i = 0; i < n; i++) {
            sum_r += r[i];
        }
        p.post(b, "<=", sum_r);

    }

    public static void main(String[] args) {
        FurnitureMoving p = new FurnitureMoving();
        p.define();
        p.solve();
    }
    

    // Problem definition    
    public void define() {

        Var numPersons = p.variable("numPersons", 1, 5); // will be minimized
        int maxTime    = 60;
        
        // Start times
        Var Sp = p.variable("Sp", 0, maxTime); // Piano
        Var Sc = p.variable("Sc", 0, maxTime); // Chair 
        Var Sb = p.variable("Sb", 0, maxTime); // Bed
        Var St = p.variable("St", 0, maxTime); // Table
        Var sumStartTimes = p.variable("SumStartTimes", 0, 1000);

        Var[] starts = {Sp,Sc,Sb,St};
        p.post(p.sum(starts), "=", sumStartTimes);
        
        Var[] endTimes      = new Var[4];
        int[] durations = {30,10,15,15}; // duration of task
        int[] resources = {3,1,3,2};     // resources: num persons required for each task
        for (int i = 0; i < durations.length; i++) {
            endTimes[i]  = p.variable("end-"+i, 0, maxTime);
            p.post(starts[i].plus(durations[i]), "=",  endTimes[i]);
        }
    
        my_cumulative(starts, durations, resources, numPersons);
        
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
        // solver.traceExecution(true);        

        //
        // solve
        //

        
        Solution s = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("numPersons"));
        if (s == null) {
            p.log("No solution");
        } else {

            System.out.println("\nNumber of persons needed: " + s.getValue("numPersons") + ":");
            System.out.println(
                               "Piano: " + s.getValue("Sp") + ".." + s.getValue("end-0") + "\n" +
                               "Chair: " + s.getValue("Sc") + ".." + s.getValue("end-1") + "\n" +
                               "Bed  : " + s.getValue("Sb") + ".." + s.getValue("end-2") + "\n" + 
                               "Table: " + s.getValue("St") + ".." + s.getValue("end-3")
                               );

            System.out.println();

            // s.log();
        }
        solver.logStats();
    }

}
