package org.jcp.jsr331.hakan;

/**
  *
  * Global constraint alldifferent_except_0 (decomposition) in JSR-331.
  *
  *
  * From Global Constraint Catalog
  * http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  * """
  * Enforce all variables of the collection VARIABLES to take distinct values, 
  * except those variables that are assigned to 0.
  * """
  *
  * 
  * Compare with the following models:
  * - Comet: http://www.hakank.org/comet/alldifferent_except_0.co
  * - ECLiPSE: http://www.hakank.org/eclipse/alldifferent_except_0.ecl
  * - Gecode: http://www.hakank.org/gecode/alldifferent_except_0.cpp
  * - Gecode/R: http://www.hakank.org/gecode_r/all_different_except_0.rb
  * - Google CP Solver: http://www.hakank.org/google_or_tools/alldifferent_except_0.py
  * - MiniZinc: http://www.hakank.org/minizinc/alldifferent_except_0.mzn
  * - SICStus: http://www.hakank.org/sicstus/alldifferent_except_0.pl
  * - Tailor/Essence': http://www.hakank.org/tailor/alldifferent_except_0.eprime
  * - Zinc: http://www.hakank.org/minizinc/alldifferent_except_0.zinc
  *
  * This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my JSR-331 page: http://www.hakank.org/jsr_331/ 
  *
  */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class AllDifferentExcept0_test {

    int n;

    Problem p = ProblemFactory.newProblem("AllDifferentExcept0_test");

    //
    // decomposition of alldifferent except 0
    //
    public void allDifferentExcept0(Var[] v) {
        
        // AllDifferentExceptC(v, 0);
        allDifferentExceptC(v, 0);

    }

    //
    // slightly more general: alldifferent except c
    //
    public void allDifferentExceptC(Var[] v, int c) {
        int len = v.length;

        for(int i = 0; i < v.length; i++) {
            for(int j = i+1; j < v.length; j++) {
                // if v[i] > 0 && v[j] > 0 -> v[i] != v[j]
                Constraint c1 = p.linear(v[i],"!=", c);
                Constraint c2 = p.linear(v[j],"!=", c);
                Constraint c3 = p.linear(v[i],"!=", v[j]);
                p.postIfThen(c1.and(c2), c3);

            }
        }
        
    }

    //
    // very simple decomposition of increasing constraint
    //
    public void increasing(Var[] v) {
        for(int j = 1; j < v.length; j++) {
            p.post(v[j], ">=", v[j-1]);
        }
    }


    // main
    public static void main(String[] args) {

        AllDifferentExcept0_test pp = new AllDifferentExcept0_test();
        pp.define();
        pp.solve();
    }
    

    // Problem definition    
    public void define() {

        n = 6;

        //
        // variables
        //
        Var[] x = p.variableArray("x", 0, n, n);

        //
        // constraints
        //
        increasing(x);

        // All values in x should be different
        allDifferentExcept0(x);

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
            for(int i = 0; i < n; i++) {
                System.out.print(s.getValue("x-"+i) + " ");
            }
            System.out.println();
            // s.log();
        }

        solver.logStats();
    }

}
