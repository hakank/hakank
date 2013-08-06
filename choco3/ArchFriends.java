/**
 *
 * Arch Friends puzzle (Dell Logic Puzzles) in Choco3.
 *
 * Problem formulation from 
 * http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
 * """
 * Title: Arch Friends
 * Author: Mark T. Zegarelli
 * Publication: Dell Logic Puzzles
 * Issue: April, 1998
 * Page: 7
 * Stars: 1
 *
 * Harriet, upon returning from the mall, is happily describing her four shoe 
 * purchases to her friend Aurora. Aurora just loves the four different kinds 
 * of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
 * and suede sandals), but Harriet can't recall at which different store (Foot 
 * Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
 * Can you help these two figure out the order in which Harriet bought each 
 * pair of shoes, and where she bought each?
 *
 *  1. Harriet bought fuchsia flats at Heels in a Handcart.
 *  2. The store she visited just after buying her purple pumps was not Tootsies.
 *  3. The Foot Farm was Harriet's second stop.
 *  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
 *
 * Determine: Order - Shoes - Store 
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

import java.util.*;

public class ArchFriends extends AbstractProblem {

  int n = 4;

  IntVar[] shoes;
  IntVar[] shoes_inv;
  IntVar[] shops;
  IntVar[] shops_inv;

  @Override
  public void buildModel() {

    shoes = VariableFactory.enumeratedArray("shoes", n, 0, n-1, solver);
    IntVar ecru_espadrilles = shoes[0];
    IntVar fuchsia_flats    = shoes[1];
    IntVar purple_pumps     = shoes[2];
    IntVar suede_sandals    = shoes[3];
    shoes_inv = VariableFactory.enumeratedArray("shoes_inv", n, 0, n-1, solver);

    shops = VariableFactory.enumeratedArray("shoes", n, 0, n-1, solver);    
    IntVar foot_farm           = shops[0];
    IntVar heels_in_a_handcart = shops[1];
    IntVar the_shoe_palace     = shops[2];
    IntVar tootsies            = shops[3];
    shops_inv = VariableFactory.enumeratedArray("shops_inv", n, 0, n-1, solver);

    solver.post(IntConstraintFactory.alldifferent(shoes, "BC"));
    solver.post(IntConstraintFactory.alldifferent(shops, "BC"));

    // 1. Harriet bought fuchsia flats at Heels in a Handcart.
    solver.post(IntConstraintFactory.arithm(fuchsia_flats, "=", heels_in_a_handcart));

    // 2. The store she visited just after buying her purple
    //    pumps was not Tootsies.
    solver.post(IntConstraintFactory.arithm(VariableFactory.offset(purple_pumps, 1), "!=", tootsies));

    //  3. The Foot Farm was Harriet's second stop.
    solver.post(IntConstraintFactory.arithm(foot_farm, "=",1));

    // 4. Two stops after leaving The Shoe Place, Harriet 
    //    bought her suede sandals.
    solver.post(IntConstraintFactory.arithm(VariableFactory.offset(the_shoe_palace,2), "=", suede_sandals));


    solver.post(IntConstraintFactory.inverse_channeling(shoes, shoes_inv, 0, 0));
    solver.post(IntConstraintFactory.inverse_channeling(shops, shops_inv, 0, 0));

  }


  @Override
  public void createSolver() {
    solver = new Solver("ArchFriends");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(shoes, shops))); 
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
        System.out.print("shoes: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", shoes[i].getValue());
        }
        System.out.print("  shoes_inv: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", shoes_inv[i].getValue());
        }
        System.out.println();

        System.out.print("shops: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", shops[i].getValue());
        }
        System.out.print("  shops_inv: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", shops_inv[i].getValue());
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

    new ArchFriends().execute(args);

  }

}

