package org.jcp.jsr331.hakan;


/**
 *
 * All interval problem in JSR-331.
 *
 * CSPLib problem number 7
 * http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
 * '''
 * Given the twelve standard pitch-classes (c, c , d, ...), represented by 
 * numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
 * once and in which the musical intervals between neighbouring notes cover 
 * the full set of intervals from the minor second (1 semitone) to the major 
 * seventh (11 semitones). That is, for each of the intervals, there is a 
 * pair of neigbhouring pitch-classes in the series, between which this 
 * interval appears. The problem of finding such a series can be easily 
 * formulated as an instance of a more general arithmetic problem on Z_n, 
 * the set of integer residues modulo n. Given n in N, find a vector 
 * s = (s_1, ..., s_n), such that (i) s is a permutation of 
 * Z_n = {0,1,...,n-1}; and (ii) the interval vector 
 * v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
 * Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
 * called an all-interval series of size n; the problem of finding such 
 * a series is the all-interval series problem of size n. We may also be 
 * interested in finding all possible series of a given size. 
 * '''
 *
 * Compare with the following models:
 * - MiniZinc: http://www.hakank.org/minizinc/all_interval.mzn
 * - Comet   : http://www.hakank.org/comet/all_interval.co 
 * - Gecode/R: http://www.hakank.org/gecode_r/all_interval.rb
 * - ECLiPSe : http://www.hakank.org/eclipse/all_interval.ecl
 * - SICStus : http://www.hakank.org/sicstus/all_interval.pl
 * - Google CP Solver: http://hakank.org/google_or_tools/all_interval.py
 *
 * Model created by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class AllInterval {

    int n;
    Var[] x;
  Problem p = ProblemFactory.newProblem("All Interval");

    // main
    public static void main(String[] args) {

        int n_in = 10;

        if (args.length >= 1) {
            n_in = Integer.parseInt(args[0]);
        }

        System.out.println("\nn: " + n_in + "\n");
        AllInterval allInterval = new AllInterval();
        allInterval.define(n_in);
        allInterval.solve();

    }


    // Problem definition    
    public void define(int n_in) {

        n = n_in;
        x = p.variableArray("x", 1, n, n);
        Var[] diffs = p.variableArray("diffs", 1, n-1, n-1);
        /*
        Var[] diffs = new Var[n-1];
        for(int i = 0; i < n-1; i++) {
            diffs[i] = new javax.constraints.impl.Var(x[0].getProblem(), "diffs-"+i, 1, n-1);
        }
        */

        p.postAllDifferent(x);
        p.postAllDifferent(diffs);

        for(int k = 0; k < n-1; k++) {
            p.post(diffs[k],"=", x[k+1].minus(x[k]).abs());
        }

        // symmetry breaking
        p.post(x[0], "<", x[n-1]);
        p.post(diffs[0], "<", diffs[1]);

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(x);

        // strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
        // strategy.setVarSelectorType(VarSelectorType.MIN_VALUE);
        // strategy.setVarSelectorType(VarSelectorType.MAX_VALUE);
        // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN);
        // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MIN_VALUE);
        // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_RANDOM);
        // strategy.setVarSelectorType(VarSelectorType.RANDOM);
        // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MAX_DEGREE);
        // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_DEGREE);
        strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_WEIGHTED_DEGREE);
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
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            // s.log();
            for(int i = 0; i < n; i++) {
                System.out.print(s.getValue("x-"+i) + " ");
            }
            System.out.println();

        }

        System.out.println("\nIt was " + num_sols + " solutions.\n");

        solver.logStats();
    }

}
