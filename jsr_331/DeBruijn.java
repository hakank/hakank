package org.jcp.jsr331.hakan;

/**
  *
  * de Bruijn sequences in JSR-331.
  *
  * Both "normal" and "arbitrary" de Bruijn sequences.
  * 
  * This is a port from my MiniZinc model
  *    http://www.hakank.org/minizinc/debruijn_binary.mzn
  *
  * and is explained somewhat in the swedish blog post
  * "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  * http://www.hakank.org/webblogg/archives/001209.html
  *
  * Related programs:
  * - "Normal" de Bruijn sequences
  *  CGI program for calculating the sequences
  *  http://www.hakank.org/comb/debruijn.cgi
  *  http://www.hakank.org/comb/deBruijnApplet.html (as Java applet)
  *
  * - "Arbitrary" de Bruijn sequences
  *   Program "de Bruijn arbitrary sequences"
  *   http://www.hakank.org/comb/debruijn_arb.cgi
  *
  *   This (swedish) blog post explaines the program:
  *   "de Bruijn-sekvenser av godtycklig längd"
  *   http://www.hakank.org/webblogg/archives/001114.html
  *
  * Compare with the following models:
  * - Choco: http://www.hakank.org/choco/Debruijn.java
  * - Comet: http://www.hakank.org/comet/debruijn.co
  * - ECLiPSE: http://www.hakank.org/eclipse/debruijn.ecl
  * - Gecode/R: http://www.hakank.org/gecode_r/debruijn_binary.rb
  * - Gecode: http://www.hakank.org/gecode/debruijn.cpp
  * - Google CP Solver: http://www.hakank.org/google_or_tools/debruijn_binary.py
  * - JaCoP: http://www.hakank.org/JaCoP/DeBruijn.java
  * - MiniZinc: http://www.hakank.org/minizinc/debruijn_binary.mzn
  * - SICStus: http://www.hakank.org/sicstus/debruijn.pl
  * - Tailor/Essence': http://www.hakank.org/tailor/debruijn.eprime
  * - Zinc: http://www.hakank.org/minizinc/debruijn_binary.zinc
  *
  * JSR-331 model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */
//import javax.constraints.Solver.Objective;
// import javax.constraints.Solver.ProblemState;
// import javax.constraints.Problem;
// import javax.constraints.Constraint;
// import javax.constraints.ProblemFactory;
import javax.constraints.*;
// import javax.constraints.impl.search.StrategyLogVariables;
// import javax.constraints.VarSelector.VarSelectorType;
// import javax.constraints.ValueSelector.ValueSelectorType;

import java.io.*;
import java.util.*;
import java.text.*;

public class DeBruijn {
  //
  // Note: The names is the same as the MiniZinc model for
  // easy comparison.
  //
  
  // These parameters may be set by the user:
  //  - base 
  //  - n 
  //  - m
  int base;          // the base to use. Also known as k. 
  int n;             // number of bits representing the numbers 
  int m;             // length of the sequence, defaults to m = base^n
  int num_solutions; // number of solutions to show, default all
  
  Problem p = ProblemFactory.newProblem("deBruijn");
  
  // integer power method
  static int pow( int x, int y) {
    int z = x; 
    for( int i = 1; i < y; i++ ) z *= x;
    return z;
  } 
  
  // main
  public static void main(String[] args) {
    
    int base = 2;
    int n = 3;        
    int m = 0;
    int num_solutions = 0;
    
    if (args.length >= 4) {
      num_solutions = Integer.parseInt(args[3]);
    }
    
    if (args.length >= 3) {
      m = Integer.parseInt(args[2]);
    }
    if (args.length >= 2) {
      base = Integer.parseInt(args[0]);
      n = Integer.parseInt(args[1]);
    }
    
    DeBruijn pp = new DeBruijn();
    pp.define(base, n, m, num_solutions);
    pp.solve();
  }
  
  
    // Problem definition    
    public void define(int in_base, int in_n, int in_m, int in_num_solutions) {

        base = in_base;
        n = in_n;
        int pow_base_n = pow(base,n); // base^n, the range of integers
        m = pow_base_n;
        if (in_m > 0) {
            if (in_m > m) {
                p.log("m must be <= base^n (" + m + ")");
                System.exit(1);
            }
            m = in_m;            
        }
        num_solutions = in_num_solutions;

        p.log("Using base: " + base + " n: " + n + " m: " + m + " (num_solutions: " + num_solutions +")");
         
        // decimal representation, ranges from 0..base^n-1
        Var[] x = p.variableArray("x", 0, pow_base_n-1, m);

        //
        // convert between decimal number in x[i] and "base-ary" numbers 
        // in binary[i][0..n-1].
        //
        // (This corresponds to the predicate toNum in the MiniZinc model)
        //

        // calculate the weights array
        int[] weights = new int[n];
        int w = 1;
        for(int i = 0; i < n; i++) {
            weights[n-i-1] = w;
            w *= base;            
        }

        // connect binary <-> x
        Var[][] binary = new Var[m][n];
        for(int i = 0; i < m; i++) {
            binary[i] = p.variableArray("binary" + i, 0, base-1, n);
            p.post(x[i], "=", p.scalProd(weights, binary[i]));
        }

        //
        // assert the the deBruijn property:  element i in binary starts
        // with the end of element i-1
        //
        for(int i = 1; i < m; i++) {
            for(int j = 1; j < n; j++) {
                p.post(binary[i-1][j],"=", binary[i][j-1]);
            }
        }

        // ... "around the corner": last element is connected to the first
        for(int j = 1; j < n; j++) {
            p.post(binary[m-1][j], "=", binary[0][j-1]);
        }

        //
        // This is the de Bruijn sequence, i.e.
        // the first element of of each row in binary[i]
        //
        Var[] bin_code = new Var[m];
        for(int i = 0; i < m; i++) {
            bin_code[i] = p.variable("bin_code-" + i, 0, base-1);
            p.post(bin_code[i], "=", binary[i][0]);
        }

        
        // All values in x should be different
        p.postAllDifferent(x);

        // Symmetry breaking: the minimum value in x should be the
        // first element.
        p.postMin(x, "=", x[0]);

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
        Solution[] res = solver.findAllSolutions();
        for(int k = 0; k < res.length; k++) {
            num_sols++;
            Solution s = res[k];
            System.out.println("\nSolution #" + num_sols);

            System.out.print("x (decimal values):");
            for (int i = 0; i < m; i++) {
                System.out.print(s.getValue("x-"+i) + " ");
            }

            System.out.print("\nde Bruijn sequence: ");
            for(int i = 0; i < m; i++) {
                System.out.print(s.getValue("bin_code-"+i) + " ");
            }
                        
            System.out.println("\nbinary:");
            for(int i = 0; i < m; i++) {
                for(int j = 0; j < n; j++) {
                    System.out.print(s.getValue("binary"+i+"-"+j) + " ");
                }
                System.out.println(": " + s.getValue("x-"+i));
            }

            p.log("\n");       
            
            // check number of solutions
            p.log("num_solutions: " + num_solutions + " num_sols: " + num_sols);
            if (num_solutions > 0 && num_sols >= num_solutions) {
                break;
            }

        }
        /*
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            // s.log();
            
            System.out.println("\nSolution #" + num_sols);

            System.out.print("x (decimal values):");
            for (int i = 0; i < m; i++) {
                System.out.print(s.getValue("x-"+i) + " ");
            }

            System.out.print("\nde Bruijn sequence: ");
            for(int i = 0; i < m; i++) {
                System.out.print(s.getValue("bin_code-"+i) + " ");
            }
                        
            System.out.println("\nbinary:");
            for(int i = 0; i < m; i++) {
                for(int j = 0; j < n; j++) {
                    System.out.print(s.getValue("binary"+i+"-"+j) + " ");
                }
                System.out.println(": " + s.getValue("x-"+i));
            }

            p.log("\n");       
            
            // check number of solutions
            p.log("num_solutions: " + num_solutions + " num_sols: " + num_sols);
            if (num_solutions > 0 && num_sols >= num_solutions) {
                break;
            }
        }
        */

        solver.logStats();
    }

}
