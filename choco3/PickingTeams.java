/**
 *
 * Picking teams in Choco3
 *
 * This model was inspired by David Curran's
 * blog post "The Fairest Way to Pick a Team "
 * http://liveatthewitchtrials.blogspot.se/2012/06/fairest-way-to-pick-team.html
 * """
 * What is the best way to pick a team? As kids we would always strictly alternate 
 * between teams so team 1 had first team 2 the second pick and then team 1 again etc.
 * 
 * Most things you can measure about people are on a bell curve. A small number of 
 * people are bad, most are in the middle and a few are good. There are a few good 
 * known metrics of ability. None are perfect, there is no one number that can sum up 
 * ability. The simpler the sport the more one metric can tell you, in cycling VO2 max is 
 * a very good indicator. Whereas in soccer VO2 max, kicking speed, vertical leap, number 
 * of keep me ups you can do etc could all measure some part of football ability.
 * 
 * So say there was one good metric for a task and teams were picked based on this. 
 * Is the standard strict alteration, where Team 1 picks then Team 2 alternating, fair? 
 * Fair here meaning both teams end up with a similar quality. 
 * """
 *
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * http://www.hakank.org/choco3/
 *
 */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.selectors.values.InDomainMax;
import solver.search.strategy.selectors.values.InDomainMiddle;
import solver.search.strategy.selectors.values.InDomainMin;
import solver.search.strategy.selectors.values.InDomainRandom;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import solver.search.strategy.strategy.Assignment;
import util.ESat;
import util.tools.ArrayUtils;
import util.tools.StatisticUtils;

import java.util.*;
import java.util.Random;

public class PickingTeams extends AbstractProblem {

  @Option(name = "-n", usage = "Number of people (default 10).", required = false)
  int n = 10;

  @Option(name = "-max", aliases = "-maxval", usage = "Max strength value (default 10).", required = false)
  int maxval = 10;

  @Option(name = "-show_all", usage = "Show all the data for the teams (default false).", required = false)
  boolean show_all = false;

  @Option(name = "-standard", usage = "Use the standard problem (default false).", required = false)
  boolean standard = false;

  @Option(name = "-use_activity", usage = "Use ActivityBased strategy (default false).", required = false)
  boolean use_activity = false;

  int teams = 2;

  // Array of "strength" of each member.
  int[] s; 

  
  IntVar[] x;
  IntVar[] sums;
  IntVar diff;

  @Override
  public void buildModel() {

    if (n % teams != 0) {
      System.out.println("The number of people (n) must be divisible by number of teams (" + teams + ").");
      System.exit(1);
    }

    s = new int[n];
    Random rand = new Random();
    for(int i = 0; i < n; i++) {
      s[i] = 1+rand.nextInt(maxval-1); 
    }

    if (standard) {
      s = new int[] {35, 52, 17, 26, 90, 55, 57, 54, 41, 9, 75, 24, 17, 23, 62, 74, 100, 67, 40, 48, 7, 6, 44, 19, 16, 14, 2, 66, 70, 2, 43, 45, 76, 53, 90, 12, 88, 96, 30, 30, 36, 93, 74, 1, 52, 45, 38, 7, 24, 96, 17, 21, 12, 12, 23, 90, 77, 64, 37, 79, 67, 62, 24, 11, 74, 82, 51, 17, 72, 18, 37, 94, 43, 44, 32, 86, 94, 33, 97, 27, 38, 38, 29, 92, 35, 82, 22, 66, 80, 8, 62, 72, 25, 13, 94, 42, 51, 31, 69, 66};
      n = s.length;
    }


    int the_sum = StatisticUtils.sum(s);
    int half_sum = the_sum / 2;

    // Don't print too large sets...
    if (n < 100 || show_all) {
      System.out.print("s: ");
      for(int i = 0; i < n; i++) {
        System.out.print(s[i] + " ");
      }
      System.out.println();
    }


    x = VariableFactory.boundedArray("x", n, 0, teams-1, solver);
    diff = VariableFactory.bounded("diff", 0, half_sum, solver);
    sums = VariableFactory.boundedArray("sums", 2, 0, the_sum, solver);


    // Ensure that there are the same number of people in each team
    int n2 = (int)n / 2;
    System.out.println("people in each team: " + n2);

    IntVar n2Var = VariableFactory.fixed(n2, solver);
    for(int i = 0; i < 2; i++) {
      solver.post(IntConstraintFactory.count(i,x,n2Var));
    }

    // Calculate the differences in strength between the two teams.
    for(int k = 0; k < 2; k++) {
      BoolVar[] bteam = VariableFactory.boolArray("bteam", n, solver);
      for(int i = 0; i < n; i++) {
        solver.post(IntConstraintFactory.implies(bteam[i],
                                                 IntConstraintFactory.arithm(x[i],"=",k)));
        solver.post(IntConstraintFactory.implies(VariableFactory.not(bteam[i]),
                                                 IntConstraintFactory.arithm(x[i],"!=",k)));
      }
      solver.post(IntConstraintFactory.scalar(bteam, s, sums[k]));
    }
    solver.post(IntConstraintFactory.distance(sums[0], sums[1], "=", diff));


    // symmetry breaking: assign first number to team 0
    solver.post(IntConstraintFactory.arithm(x[0],"=",VariableFactory.fixed(0, solver)));

    // Odd sum must yield odd diff and even sum yield even diff
    IntVar even_odd = VariableFactory.fixed(the_sum % 2, solver);
    IntVar c2 = VariableFactory.fixed(2, solver);
    solver.post(IntConstraintFactory.mod(diff, c2, even_odd));


  }

  @Override
  public void createSolver() {
    solver = new Solver("PickingTeams");
  }

  @Override
  public void configureSearch() {
    if (use_activity) {
      solver.set(IntStrategyFactory.ActivityBased(x, solver, 0.999d, 0.9d, 8, 1.1d, 1, seed)); 
    } else {
      solver.set(new ImpactBased(x, 2, 3, 10, seed, false)); // original
      // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(x, seed));
      // solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
      // solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));

    }

  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, diff);

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("sums:");
        for(int k = 0; k < 2; k++) {
          System.out.print("Team " + k + ": " + sums[k].getValue() + " ");
        }
        System.out.println("  diff: " + diff.getValue());
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.println();

        if (n <= 100 || show_all) {
          System.out.println("Member in each team and their strengths:");
          for(int k = 0; k < 2; k++) {
            System.out.print("Team " + k + ": ");
            for(int i = 0; i < n; i++) {
              if (x[i].getValue() == k) {
                System.out.format("%d(%d) ", i, s[i]);
              }
            }
            System.out.println();
          }
          System.out.println();
          System.out.println("  diff: " + diff.getValue() + "\n");
        }


        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);

      System.out.println("It was " + num_solutions + " solutions.");

    } else {

      System.out.println("Problem is not feasible.");

    }

  }


  //
  // main
  //
  public static void main(String args[]) {

    new PickingTeams().execute(args);

  }
}

