package org.jcp.jsr331.hakan;

/**
 *
 * Young tableaux in JSR-331.
 *
 * See 
 * http://mathworld.wolfram.com/YoungTableau.html
 * and
 * http://en.wikipedia.org/wiki/Young_tableau
 * """
 * The partitions of 4 are
 *  {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 *
 * And the corresponding standard Young tableaux are:
 *
 * 1.   1 2 3 4
 *
 * 2.   1 2 3         1 2 4    1 3 4
 *      4             3        2
 *
 * 3.   1 2           1 3
 *      3 4           2 4
 *
 * 4    1 2           1 3      1 4 
 *      3             2        2 
 *      4             4        3
 *
 * 5.   1
 *      2
 *      3
 *      4
 * """  
 * 
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/YoungTableaux.java
 * - Comet: http://www.hakank.org/comet/young_tableaux.co
 * - ECLiPSE: http://www.hakank.org/eclipse/young_tableaux.ecl
 * - Gecode: http://www.hakank.org/gecode/young_tableaux.cpp
 * - Google CP Solver: http://www.hakank.org/google_or_tools/young_tableaux.py
 * - JaCoP: http://www.hakank.org/JaCoP/YoungTableaux.java
 * - MiniZinc: http://www.hakank.org/minizinc/young_tableaux.mzn
 * - SICStus: http://www.hakank.org/sicstus/young_tableaux.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/young_tableaux.eprime
 * - Zinc: http://www.hakank.org/minizinc/young_tableaux.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 */
import javax.constraints.*;

public class YoungTableaux {
	
	Problem p = ProblemFactory.newProblem("TestXYZ");

    int n;
    Var[][] x; // matrix
    Var[] x_a; // x as an array, for Count
    Var[] partition; // partition structure.

    Var[] gcc_count; // Instead of global constraint count

    public void increasing(Var[] v) {

        for(int j = 1; j < n; j++) {
            p.post(v[j], ">=", v[j-1]);
        }
    }


    // main
    public static void main(String[] args) {

       int n_tmp = 4;
        if (args.length == 1) {
            n_tmp = Integer.parseInt(args[0]);
        }

        YoungTableaux yt = new YoungTableaux();
        yt.define(n_tmp);
        yt.solve();

    }
    

    // Problem definition    
    public void define(int n_tmp) {

        n = n_tmp;

        x = new Var[n][n];
        x_a = new Var[n*n];
        partition = new Var[n];   // the partition structure.

        //
        // Value n+1 in x will be replaced with "_" in the output.
        // This representation simplifies the "increasing" constraints below.
        for(int i = 0; i < n; i++) {
            partition[i] = p.variable("partition-" + i, 0, n+1);
            for(int j = 0; j < n; j++) {
                x[i][j]= p.variable("x-" + i + "-" + j, 1, n+1);
                x_a[n*i + j]= x[i][j];
            }
        }
        
        // 1..n is used exactly once in the whole matrix. 
        // n+1, however will be used n^2 - n times.
        for(int i = 1; i <= n; i++) {
            p.postCardinality(x_a, i, "=", 1);
        } 

        // x[0][0] = 1
        p.post(x[0][0], "=", 1);

        //
        // increasing row wise
        //
        for(int i = 0; i < n; i++) {
            increasing(x[i]);
        }

        //
        // increasing column wise
        //
        for(int j = 0; j < n; j++) {
            for(int i = 1; i < n; i++) {
            	p.post(x[i][j], ">=", x[i-1][j]);
            }
        }

        // calculate the partition structure:
        // for each row: sum number of entries where x[i][j] between 1.. n
        // i.e. ignore those that is n+1
        for(int i = 0; i < n; i++) {
            Var[] p_bin = new Var[n];
            for(int j = 0; j < n; j++) {
                p_bin[j] = p.variable("p_bin-" + j, 0, 1);
            }
            // sum all entries where x[i][j] <= n
            for(int j = 0; j < n; j++) {
                // There is no IfThenElse
            	p.postIfThen(p.linear(x[i][j], "<=", n),
                		     p.linear(p_bin[j], "=", 1)
                            );
            	p.postIfThen(p.linear(x[i][j], ">", n),
                		     p.linear(p_bin[j], "=", 0)
                            );

            }
            p.post(p_bin, "=", partition[i]);
        }

        p.post(partition, "=", n);

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
        int maxNumberOfSolutions = 5;
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            // Tableaux
            System.out.println("\nTableaux solution# :" + num_sols);
            for(int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    int r = s.getValue("x-"+i+"-"+j);
                    if (r == n+1) {
                        System.out.print("_" + " ");                        
                    } else {
                        System.out.print(r + " ");
                    }
                }
                System.out.println();
            }
            
            // Partition
            System.out.println("Partition: ");
            for(int i = 0; i < n; i++) {
                System.out.print(s.getValue("partition-"+i) + " ");
            }
            System.out.println("\n");
            
            // s.log();
            if (num_sols >= maxNumberOfSolutions)
            	break;
        }

        solver.logStats();
    }

}
