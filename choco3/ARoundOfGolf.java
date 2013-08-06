/**
 *
 * A round of golf puzzle (Dell Logic Puzzles) in Choco3.
 *
 * From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
 * """
 * Title: A Round of Golf
 * Author: Ellen K. Rodehorst
 * Publication: Dell Favorite Logic Problems
 * Issue: Summer, 2000
 * Puzzle #: 9
 * Stars: 1
 *
 * When the Sunny Hills Country Club golf course isn't in use by club members,
 * of course, it's open to the club's employees. Recently, Jack and three other
 * workers at the golf course got together on their day off to play a round of
 * eighteen holes of golf.
 * Afterward, all four, including Mr. Green, went to the clubhouse to total
 * their scorecards. Each man works at a different job (one is a short-order
 * cook), and each shot a different score in the game. No one scored below
 * 70 or above 85 strokes. From the clues below, can you discover each man's
 * full name, job and golf score?
 *
 * 1. Bill, who is not the maintenance man, plays golf often and had the lowest
 * score of the foursome.
 * 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten
 * strokes more than the pro-shop clerk.
 * 3. In some order, Frank and the caddy scored four and seven more strokes than
 * Mr. Sands.
 * 4. Mr. Carter thought his score of 78 was one of his better games, even
 *    though Frank's score  was lower.
 * 5. None of the four scored exactly 81 strokes.
 *
 * Determine: First Name - Last Name - Job - Score
 * """
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

import java.util.*;

public class ARoundOfGolf extends AbstractProblem {

  int n = 4;

  int Jack  = 0;
  int Bill  = 1;
  int Paul  = 2;
  int Frank = 3;
  
  IntVar[] last_name;
  IntVar[] job;
  IntVar[] score;


  @Override
  public void buildModel() {

    last_name = VariableFactory.enumeratedArray("last_name", n, 0, n-1, solver);
    job = VariableFactory.enumeratedArray("last_name", n, 0, n-1, solver);
    score = VariableFactory.enumeratedArray("last_name", n, 70, 85, solver);

    // IntVar JackVar = VariableFactory.fixed(Jack, solver);
    IntVar BillVar = VariableFactory.fixed(Bill, solver);
    IntVar PaulVar = VariableFactory.fixed(Paul, solver);
    IntVar FrankVar = VariableFactory.fixed(Frank, solver);

    // IntVar Green  = last_name[0]; // not used
    IntVar Clubb  = last_name[1];
    IntVar Sands  = last_name[2];
    IntVar Carter = last_name[3];

    // IntVar cook            = job[0]; // not used
    IntVar maintenance_man = job[1];
    IntVar clerk           = job[2];
    IntVar caddy           = job[3];


    solver.post(IntConstraintFactory.alldifferent(last_name, "BC"));
    solver.post(IntConstraintFactory.alldifferent(job, "BC"));
    solver.post(IntConstraintFactory.alldifferent(score, "BC"));

    // 1. Bill, who is not the maintenance man, plays golf often and had
    //    the lowest score of the foursome.
    solver.post(IntConstraintFactory.arithm(maintenance_man, "!=", BillVar));
    solver.post(IntConstraintFactory.arithm(score[Bill], "<", score[Jack]));
    solver.post(IntConstraintFactory.arithm(score[Bill], "<", score[Paul]));
    solver.post(IntConstraintFactory.arithm(score[Bill], "<", score[Frank]));

    // 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and
    //    scored ten strokes more than the pro-shop clerk.
    solver.post(IntConstraintFactory.arithm(Clubb, "!=", PaulVar));
    IntVar scoreClubb = VariableFactory.enumerated("scoreClubb", 70, 85, solver);
    IntVar score_clerk = VariableFactory.enumerated("score_clerk", 70, 85, solver);
    solver.post(IntConstraintFactory.element(scoreClubb, score, Clubb, 0));
    solver.post(IntConstraintFactory.element(score_clerk, score, clerk, 0));
    solver.post(IntConstraintFactory.arithm(scoreClubb, "=", VariableFactory.offset(score_clerk, 10)));


    // 3. In some order, Frank and the caddy scored four and seven more
    //    strokes than Mr. Sands.
    solver.post(IntConstraintFactory.arithm(caddy, "!=", FrankVar));
    solver.post(IntConstraintFactory.arithm(Sands, "!=", FrankVar));
    solver.post(IntConstraintFactory.arithm(caddy, "!=", Sands));

    BoolVar b3_a_1 = VariableFactory.bool("b3_a_1", solver);
    BoolVar b3_a_2 = VariableFactory.bool("b3_a_2", solver);
    BoolVar b3_b_1 = VariableFactory.bool("b3_b_1", solver);
    BoolVar b3_b_2 = VariableFactory.bool("b3_b_2", solver);
    IntVar scoreSands = VariableFactory.enumerated("scoreSands", 70, 85, solver);
    solver.post(IntConstraintFactory.element(scoreSands, score, Sands, 0));
    IntVar score_caddy = VariableFactory.enumerated("score_caddy", 70, 85, solver);
    solver.post(IntConstraintFactory.element(scoreSands, score, Sands, 0));
    solver.post(IntConstraintFactory.element(score_caddy, score, caddy, 0));

    solver.post(IntConstraintFactory.implies(b3_a_1,
                                             IntConstraintFactory.arithm(VariableFactory.offset(scoreSands, 4),
                                                                         "=",
                                                                         score[Frank])));
    solver.post(IntConstraintFactory.implies(VariableFactory.not(b3_a_1),
                                             IntConstraintFactory.arithm(VariableFactory.offset(scoreSands, 4),
                                                                         "!=",
                                                                         score[Frank])));

    solver.post(IntConstraintFactory.implies(b3_a_2,
                                             IntConstraintFactory.arithm(score_caddy,
                                                                         "=",
                                                                         VariableFactory.offset(scoreSands, 7))));
    solver.post(IntConstraintFactory.implies(VariableFactory.not(b3_a_2),
                                             IntConstraintFactory.arithm(score_caddy,
                                                                         "!=",
                                                                         VariableFactory.offset(scoreSands, 7))));


    solver.post(IntConstraintFactory.implies(b3_b_1,
                                             IntConstraintFactory.arithm(VariableFactory.offset(scoreSands, 7),
                                                                         "=",
                                                                         score[Frank])));
    solver.post(IntConstraintFactory.implies(VariableFactory.not(b3_b_1),
                                             IntConstraintFactory.arithm(VariableFactory.offset(scoreSands, 7),
                                                                         "!=",
                                                                         score[Frank])));

    solver.post(IntConstraintFactory.implies(b3_b_2,
                                             IntConstraintFactory.arithm(score_caddy,
                                                                         "=",
                                                                         VariableFactory.offset(scoreSands, 4))));
    solver.post(IntConstraintFactory.implies(VariableFactory.not(b3_b_2),
                                             IntConstraintFactory.arithm(score_caddy,
                                                                         "!=",
                                                                         VariableFactory.offset(scoreSands, 4))));

    BoolVar b3_a = VariableFactory.bool("b3_a", solver);
    BoolVar b3_b = VariableFactory.bool("b3_a", solver);
    solver.post(IntConstraintFactory.times(b3_a_1,b3_a_2, b3_a));
    solver.post(IntConstraintFactory.times(b3_b_1,b3_b_2, b3_b));
    solver.post(IntConstraintFactory.arithm(b3_a,"+", b3_b,"=", 1));


    // 4. Mr. Carter thought his score of 78 was one of his better games,
    //    even though Frank's score was lower.
    solver.post(IntConstraintFactory.arithm(Carter, "!=", FrankVar));
    IntVar scoreCarter = VariableFactory.enumerated("scoreCarter", 70, 85, solver);
    solver.post(IntConstraintFactory.element(scoreCarter, score,Carter, 0));
    solver.post(IntConstraintFactory.arithm(scoreCarter, "=", VariableFactory.fixed(78, solver)));
    solver.post(IntConstraintFactory.arithm(score[Frank], "<", scoreCarter));

    // 5. None of the four scored exactly 81 strokes.
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.arithm(score[i], "!=", 81));
    }

  }


  @Override
  public void createSolver() {
    solver = new Solver("ARoundOfGolf");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(last_name, job, score))); 
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.print("last_name: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", last_name[i].getValue());
        }
        System.out.print("\njob      : ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", job[i].getValue());
        }
        System.out.print("\nscore    : ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", score[i].getValue());
        }
        System.out.println();

          
        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new ARoundOfGolf().execute(args);

  }

}

