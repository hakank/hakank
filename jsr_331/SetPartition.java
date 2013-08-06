package org.jcp.jsr331.hakan;

/**
 *
 * Set partition problem in JSR-331.
 *
 *  
 * Problem formulation from
 *  http://www.koalog.com/resources/samples/PartitionProblem.java.html
 * """
 *  This is a partition problem.
 * Given the set S = {1, 2, ..., n},
 *  it consists in finding two sets A and B such that:
 * 
 *   A U B = S,
 *   |A| = |B|,
 *   sum(A) = sum(B),
 *   sum_squares(A) = sum_squares(B)
 *
 *"""
 *
 * This model uses a binary matrix to represent the sets.
 *
 * Compare with the following models:
 * - MiniZinc: http://www.hakank.org/minizinc/set_partition.mzn
 * - Gecode/R: http://www.hakank.org/gecode_r/set_partition.rb 
 * - Comet: http://hakank.org/comet/set_partition.co
 * - Gecode: http://hakank.org/gecode/set_partition.cpp
 * - ECLiPSe: http://hakank.org/eclipse/set_partition.ecl
 * - SICStus: http://hakank.org/sicstus/set_partition.pl
 * - Google CP Solver: http://hakank.org/google_or_tools/set_partition.py
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetPartition {

    int n;
    int num_sets = 2;

    Var[] a_flatten;
    Problem p = ProblemFactory.newProblem("SetPartition");

    // main
    public static void main(String[] args) {

        int n_in = 16;

        if (args.length > 0) {
            n_in = Integer.parseInt(args[0]);
        }

        SetPartition pp = new SetPartition();
        pp.define(n_in);
        pp.solve();


    }
    

    // Problem definition    
    public void define(int n_in) {
        
        n = n_in;

        System.out.println("n:" + n);

        Var[][] a = new Var[num_sets][n];
        a_flatten = new Var[num_sets*n];
        for(int i = 0; i < num_sets; i++) {
            for(int j = 0; j < n; j++) {
                a[i][j] = p.variable("a-"+i+"-"+j, 0, 1);
                a_flatten[i*n+j] = a[i][j];
            }
        }
        
        // partition the sets (all different)
        for(int k = 0; k < n; k++) {
            p.post(a[0][k], "!=" , a[1][k]);
        }

        for(int i = 0; i < num_sets; i++) {
            for(int j = 0; j < num_sets; j++) {
                if (i < j) {
                    Var[] s1 = new Var[n];
                    Var[] s2 = new Var[n];
                    Var[] sq1 = new Var[n];
                    Var[] sq2 = new Var[n];
                    Var[] sqsq1 = new Var[n];
                    Var[] sqsq2 = new Var[n];

                    for(int k = 0; k < n; k++) {
                        // same cardinality
                        // m.post(sum(k in 1..n) a[i,k] == sum(k in 1..n) a[j,k]);
                        // s1[k] = new javax.constraints.impl.Var(this,"s1+"+i+"-"+j+"-"+k, 0,1);
                        // s2[k] = new javax.constraints.impl.Var(this,"s2+"+i+"-"+j+"-"+k, 0,1);
                        s1[k] = p.variable("s1+"+i+"-"+j+"-"+k, 0,1);
                        s2[k] = p.variable("s2+"+i+"-"+j+"-"+k, 0,1);


                        s1[k] = a[i][k].plus(1);
                        s2[k] = a[j][k].plus(1);

                        // same sum
                        // m.post(sum(k in 1..n) k*a[i,k] == sum(k in 1..n) k*a[j,k]);
                        // sq1[k] = new javax.constraints.impl.Var(this,"sq1+"+i+"-"+j+"-"+k, 0,1);
                        // sq2[k] = new javax.constraints.impl.Var(this,"sq2+"+i+"-"+j+"-"+k, 0,1);
                        sq1[k] = p.variable("sq1+"+i+"-"+j+"-"+k, 0,1);
                        sq2[k] = p.variable("sq2+"+i+"-"+j+"-"+k, 0,1);

                        sq1[k] = (a[i][k].plus(1)).multiply(k);
                        sq2[k] = (a[j][k].plus(1)).multiply(k);

                        // same sum squared
                        // m.post((sum(k in 1..n) (k*a[i,k])^2) == (sum(k in 1..n) (k*a[j,k])^2));
                        // sqsq1[k] = new javax.constraints.impl.Var(this,"sq1+"+i+"-"+j+"-"+k, 0,1);
                        // sqsq2[k] = new javax.constraints.impl.Var(this,"sq2+"+i+"-"+j+"-"+k, 0,1);
                        sqsq1[k] = p.variable("sq1+"+i+"-"+j+"-"+k, 0,1);
                        sqsq2[k] = p.variable("sq2+"+i+"-"+j+"-"+k, 0,1);

                        sqsq1[k] = (a[i][k].plus(1)).multiply(k).power(2);
                        sqsq2[k] = (a[j][k].plus(1)).multiply(k).power(2);


                    }
                    p.post(p.sum(s1), "=", p.sum(s2));
                    p.post(p.sum(sq1), "=", p.sum(sq2));
                    p.post(p.sum(sqsq1), "=", p.sum(sqsq2));

                    // symmetry breaking
                    p.post(a[0][0],"=", 1);
    
                }

            }
        }

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(a_flatten);

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
        strategy.setVarSelectorType(VarSelectorType.MAX_DEGREE);
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

            for(int i = 0; i < num_sets; i++) {
                int sum = 0;
                for(int j = 0; j < n; j++) {
                    if (s.getValue("a-"+i+"-"+j) == 1) {
                        System.out.format("%2s ", j+1);
                        sum += j+1;
                    }
                }
                System.out.print(" = " + sum);
                System.out.println();
            }
            System.out.println();
        }
        System.out.println("It was " + num_sols + " solutions.\n");

        solver.logStats();
    }

}
