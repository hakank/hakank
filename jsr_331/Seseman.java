package org.jcp.jsr331.hakan;

/*
 * The Seseman's Convent Problem in JSR-331.
 *
 * Given the following matrix:
 *
 *    A  B  C
 *    D  _  E
 *    F  G  H
 *
 * calculate the following constraints:
 *
 *   A + B + C = RowSum
 *   A + D + F = RowSum
 *   C + E + H = RowSum
 *   F + G + H = RowSum
 *   A + B + C + D + E + F + G + H = Total
 *
 *
 * For more information about this problem:
 * - Swedish blog post with a fuller description of the problem:
 *   "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
 *   http://www.hakank.org/webblogg/archives/001084.html
 *
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/Seseman.java
 * - Comet: http://www.hakank.org/comet/seseman.co
 * - ECLiPSE: http://www.hakank.org/eclipse/seseman.ecl
 * - Gecode: http://www.hakank.org/gecode/seseman.cpp
 * - Gecode/R: http://www.hakank.org/gecode_r/seseman.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/seseman.py
 * - JaCoP: http://www.hakank.org/JaCoP/Seseman.java
 * - MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
 * - SICStus: http://www.hakank.org/sicstus/seseman.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/seseman.eprime
 * - Zinc: http://www.hakank.org/minizinc/seseman.zinc
 *
 * A CGI-program which uses the ECLiPSe program mentioned above
 *   http://www.hakank.org/seseman/seseman.cgi
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class Seseman {

    Problem p = ProblemFactory.newProblem("Seseman");

    // main
    public static void main(String[] args) {

        Seseman pp = new Seseman();
        pp.define();
        pp.solve();
    }
    

    // Problem definition    
    public void define() {

        // int start = 0; // allow empty room
        int start = 1; // don't allow empty room

        // 0..9: allow empty rooms 1..9: don't allow empty rooms
        Var A = p.variable("A", start, 9);
        Var B = p.variable("B", start, 9);
        Var C = p.variable("C", start, 9);
        Var D = p.variable("D", start, 9);
        Var E = p.variable("E", start, 9);
        Var F = p.variable("F", start, 9);
        Var G = p.variable("G", start, 9);
        Var H = p.variable("H", start, 9);

        Var [] letters = {A,B,C,D,E,F,G,H};

        Var Total = p.variable("Total", 24, 24);
        Var RowSum = p.variable("RowSum", 9, 9);


        // This is not yet implemented...
        /*
        p.post("A+B+C=RowSum", "A+B+C=RowSum");
        p.post("A+D+F=RowSum", "A+D+F=RowSum");
        p.post("C+E+H=RowSum", "C+E+H=RowSum");
        p.post("F+G+H=RowSum", "F+G+H=RowSum");
        */

        p.post(A.plus(B).plus(C), "=", RowSum);
        p.post(A.plus(D).plus(F), "=", RowSum);
        p.post(C.plus(E).plus(H), "=", RowSum);
        p.post(F.plus(G).plus(H), "=", RowSum);

        p.post(p.sum(letters), "=",Total);
 
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
            System.out.println("Solution #" + num_sols);
            System.out.println("" + s.getValue("A") + " " + s.getValue("B") + " " + s.getValue("C"));
            System.out.println("" + s.getValue("D") + " _ " + s.getValue("D"));
            System.out.println("" + s.getValue("E") + " " + s.getValue("F") + " " + s.getValue("G"));
            System.out.println("Total: " + s.getValue("Total") + " RowSum: " + s.getValue("RowSum"));
            System.out.println();

            // s.log();
        }

        solver.logStats();
    }

}
