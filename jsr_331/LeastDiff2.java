package org.jcp.jsr331.hakan;

/**
  *
  * Least Diff problem in JSR331.
  *
  * Minimize the difference ABCDE - FGHIJ
  *                     where A..J is all different in the range 0..9.
  *
  * The solution is: 50123 - 49876 = 247
  *
  * Compare with the following models:
  * - Choco: http://www.hakank.org/choco/LeastDiff2.java
  * - Comet: http://www.hakank.org/comet/least_diff.co
  * - ECLiPSE: http://www.hakank.org/eclipse/least_diff2.ecl
  * - Gecode/R: http://www.hakank.org/gecode_r/least_diff.rb
  * - Gecode: http://www.hakank.org/gecode/least_diff.cpp
  * - Google CP Solver: http://www.hakank.org/google_or_tools/least_diff.py
  * - JaCoP: http://www.hakank.org/JaCoP/LeastDiff.java
  * - MiniZinc: http://www.hakank.org/minizinc/least_diff.mzn
  * - SICStus: http://www.hakank.org/sicstus/least_diff.pl
  * - Tailor/Essence': http://www.hakank.org/tailor/leastDiff.eprime
  * - Zinc: http://www.hakank.org/minizinc/least_diff.zinc
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

public class LeastDiff2 {

    // used in solve()
    Var[] letters;
    // Var[] XArray;
    // Var[] YArray;
    Problem p = ProblemFactory.newProblem("LeastDiff");

    public static void main(String[] args) {
        LeastDiff2 leastDiff2 = new LeastDiff2();
        leastDiff2.define();
        leastDiff2.solve();
    }
    

    // Problem definition    
    public void define() {
        
        // define variables
        Var A = p.variable("A", 0, 9);
        Var B = p.variable("B", 0, 9);
        Var C = p.variable("C", 0, 9);
        Var D = p.variable("D", 0, 9);
        Var E = p.variable("E", 0, 9);
        Var F = p.variable("F", 0, 9);
        Var G = p.variable("G", 0, 9);
        Var H = p.variable("H", 0, 9);
        Var I = p.variable("I", 0, 9);
        Var J = p.variable("J", 0, 9);
        Var[] _letters = {A,B,C,D,E,F,G,H,I,J};

        letters = _letters;
        
        Var Diff = p.variable("Diff", 0, 1000);
        
        // Var X = new javax.constraints.impl.Var(this, "X", 0, 100000);
        Var X = p.variable("X", 0, 100000);
        Var[] XArray = {A,B,C,D,E};
        
        // Var Y = new javax.constraints.impl.Var(this, "Y", 0, 100000);
        Var Y = p.variable("Y", 0, 100000);
        Var[] YArray = {F,G,H,I,J};
        
        // define and post constraints
        try {
            p.postAllDifferent(letters);
            
            p.post(new int[]{10000, 1000, 100, 10, 1}, XArray, "=", X);
            p.post(new int[]{10000, 1000, 100, 10, 1}, YArray, "=", Y);
            
            p.post(X.minus(Y),"=", Diff);
            
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
        //strategy.setVars(letters);
        
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
        
        Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("Diff"));
        if (solution == null) {

            p.log("No solution");

        } else {

            solution.log();

            System.out.print(solution.getValue("A") +  "" +
                             solution.getValue("B") +  "" +
                             solution.getValue("C") +  "" +
                             solution.getValue("D") +  "" +
                             solution.getValue("E") +  "" +
                             " - " +
                             solution.getValue("F") +  "" +
                             solution.getValue("G") +  "" +
                             solution.getValue("H") +  "" +
                             solution.getValue("I") +  "" +
                             solution.getValue("J") +  "" +
                               " = ");
            System.out.println(solution.getValue("Diff"));

        }
        solver.logStats();
    }

}
