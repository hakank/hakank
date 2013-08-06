package org.jcp.jsr331.hakan;

/**
 * Send most money in Minizinc.
 *
 * From the lecture notes:
 * http://www.ict.kth.se/courses/ID2204/notes/L01.pdf
 * 
 * SEND + MOST + MONEY 
 * and maximize MONEY
 *
 * Compare with the following models:
 * - Comet: http://www.hakank.org/comet/send_most_money.co
 * - Comet: http://www.hakank.org/comet/send_most_money2.co
 * - ECLiPSE: http://www.hakank.org/eclipse/send_most_money.ecl
 * - Gecode/R: http://www.hakank.org/gecode_r/send_most_money2.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/send_most_money.py
 * - MiniZinc: http://www.hakank.org/minizinc/send_most_money.mzn
 * - SICStus: http://www.hakank.org/sicstus/send_most_money.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/send_most_money.eprime
 *
 * Model created by Hakan Kjellerstrand, hakank@bonetmail.com
 * See also my MiniZinc page: http://www.hakank.org/minizinc
 *
 */
import javax.constraints.*;

public class SendMostMoney {
	
    Problem p = ProblemFactory.newProblem("SendMostMoney");

    public void define(int MONEY) {

        Var S = p.variable( "S",1, 9);
        Var E = p.variable( "E",0, 9);
        Var N = p.variable( "N",0, 9);
        Var D = p.variable( "D",0, 9);
        Var M = p.variable( "M",1, 9);
        Var O = p.variable( "O",0, 9);
        Var T = p.variable( "T",0, 9);
        Var Y = p.variable( "Y",0, 9);
        
        Var[] vars = new Var[] { S, E, N, D, M, O, T, Y };
        // new AllDifferent(vars).post();
        p.postAllDifferent(vars);
	
        int coef[] = { 1000, 100, 10, 1, 
                       1000, 100, 10, 1, 
                       -10000, -1000, -100, -10, -1 };
        Var[] sendmostmoney = new Var[] { S, E, N, D, 
                                          M, O, S, T, 
                                          M, O, N, E, Y};
        p.post(coef, sendmostmoney, "=", 0);
        
        Var money = p.variable("money", 0, 99999);
        Var[] money_var = {M, O, N, E, Y};
        int money_coef[] = {10000, 1000, 100, 10, 1};
        p.post(money_coef, money_var, "=", money);
        
        if (MONEY > 0) {
            p.post(money, "=", MONEY);
        }
        
    }
    
    // Get optimal value of M O N E Y
    public int solve1() {

        Solver solver = p.getSolver();
        Solution s = solver.findOptimalSolution(Objective.MAXIMIZE, p.getVar("money"));

        int money = 0;
        if (s == null)
            p.log("No Solutions");
        else {
            money = s.getValue("money");
            System.out.println("Found money = " + money);
            // s.log();
            System.out.println();
        }

        solver.logStats();

        return money;
    }
    
    // Get all solutions with optimal value of M O N E Y
    public void solve2() {

        Solver solver = p.getSolver();
        SolutionIterator iter = solver.solutionIterator();
        int num_sols = 0;
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            System.out.println("  "+s.getValue("S")+s.getValue("E")+s.getValue("N")+s.getValue("D"));
            System.out.println("+ "+s.getValue("M")+s.getValue("O")+s.getValue("S")+s.getValue("T"));
            System.out.println("=======");
            System.out.println(" "+s.getValue("M")+s.getValue("O")+s.getValue("N")+s.getValue("E")+s.getValue("Y"));
            System.out.println();
            System.out.println();
            
            // s.log();
            // System.out.println();

        }
        
        solver.logStats();
    }

	
    public static void main(String[] args) {

        // first get the value of M O N E Y
        SendMostMoney p = new SendMostMoney();
        p.define(0);
        int money = p.solve1();

        // Then find all solutions for the optimal value
        SendMostMoney p2 = new SendMostMoney();
        System.out.println("\nAll solutions for money = " + money + ":");
        p2.define(money);
        p2.solve2();

    }
}
