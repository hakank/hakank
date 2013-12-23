/**
 *
 * Secret Santa problem in Choco3
 *
 * From Ruby Quiz Secret Santa
 * http://www.rubyquiz.com/quiz2.html
 * """
 * Honoring a long standing tradition started by my wife's dad, my friends
 * all play a Secret Santa game around Christmas time. We draw names and
 * spend a week sneaking that person gifts and clues to our identity. On the
 * last night of the game, we get together, have dinner, share stories, and,
 * most importantly, try to guess who our Secret Santa was. It's a crazily
 * fun way to enjoy each other's company during the holidays.
 *
 * To choose Santas, we use to draw names out of a hat. This system was
 * tedious, prone to many 'Wait, I got myself...' problems. This year, we
 * made a change to the rules that further complicated picking and we knew
 * the hat draw would not stand up to the challenge. Naturally, to solve
 * this problem, I scripted the process. Since that turned out to be more
 * interesting than I had expected, I decided to share.
 *
 * This weeks Ruby Quiz is to implement a Secret Santa selection script.
 * *  Your script will be fed a list of names on STDIN.
 * ...
 * Your script should then choose a Secret Santa for every name in the list.
 * Obviously, a person cannot be their own Secret Santa. In addition, my friends
 * no longer allow people in the same family to be Santas for each other and your
 * script should take this into account.
 * """
 *
 *  Comment: This model skips the file input and mail parts. We
 *        assume that the friends are identified with a number from 1..n,
 *        and the families is identified with a number 1..num_families. 
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
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class SecretSanta extends AbstractProblem {

  @Option(name = "-solutions", usage = "Number of show solutions (default 1).", required = false)
  int solutions = 1;


  int[] family = {1,1,1,1, 2, 3,3,3,3,3, 4,4};
  int n = family.length;

  int num_families = 4;
  
  IntVar[] x;

  @Override
  public void buildModel() {

    x = VariableFactory.boundedArray("x", n, 0, n-1, solver);

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));


    // Can't be one own"s Secret Santa.
    // (i.e. ensure that there are no fix-point in the array.)
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.arithm(x[i],"!=", VariableFactory.fixed(i, solver)));
    }
    
    
    // No Secret Santa to a person in the same family.
    for(int i = 0; i < n; i++) {
      IntVar f = VariableFactory.enumerated("family[x["+i+"]]", 1, num_families, solver);
      solver.post(IntConstraintFactory.element(f, family, x[i], 0, "detect"));
      solver.post(IntConstraintFactory.arithm(f,"!=", VariableFactory.fixed(family[i], solver)));      
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("SecretSanta");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    solver.findSolution();
    // solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("x: ");
        for(int i = 0; i < n; i++) {
          int v = x[i].getValue();
          System.out.format("%2d (family %d) is a Secret Santa of %2d (family %d)\n", i, family[i], v, family[v]);
        }
        System.out.println();

        num_solutions++;

        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

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

    new SecretSanta().execute(args);

  }
}

