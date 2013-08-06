/**
 *
 * Secret Santa problem II in Choco3
 *
 * From Maple Primes: 'Secret Santa Graph Theory'
 * http://www.mapleprimes.com/blog/jpmay/secretsantagraphtheory
 * """
 * Every year my extended family does a 'secret santa' gift exchange.
 * Each person draws another person at random and then gets a gift for
 * them. At first, none of my siblings were married, and so the draw was
 * completely random. Then, as people got married, we added the restriction
 * that spouses should not draw each others names. This restriction meant
 * that we moved from using slips of paper on a hat to using a simple
 * computer program to choose names. Then people began to complain when
 * they would get the same person two years in a row, so the program was
 * modified to keep some history and avoid giving anyone a name in their
 * recent history. This year, not everyone was participating, and so after
 * removing names, and limiting the number of exclusions to four per person,
 * I had data something like this:
 *
 * Name: Spouse, Recent Picks
 *
 * Noah: Ava. Ella, Evan, Ryan, John
 * Ava: Noah, Evan, Mia, John, Ryan
 * Ryan: Mia, Ella, Ava, Lily, Evan
 * Mia: Ryan, Ava, Ella, Lily, Evan
 * Ella: John, Lily, Evan, Mia, Ava
 * John: Ella, Noah, Lily, Ryan, Ava
 * Lily: Evan, John, Mia, Ava, Ella
 * Evan: Lily, Mia, John, Ryan, Noah
 * """
 *
 * Note: I interpret this as the following three constraints:
 * 1) One cannot be a Secret Santa of one's spouse
 * 2) One cannot be a Secret Santa for somebody two years in a row
 * 3) Optimization: maximize the time since the last time
 *
 * This model also handle single persons, something the original
 * problem don't mention.
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
import solver.constraints.IntConstraintFactory.*;
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.limits.FailLimit;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class SecretSanta2 extends AbstractProblem {

  @Option(name = "-single", usage = "Problem instance with a single person (default false).", required = false)
  boolean single = false;

  int Noah   = 0;
  int Ava    = 1;
  int Ryan   = 2;
  int Mia    = 3;
  int Ella   = 4;
  int John   = 5;
  int Lily   = 6;
  int Evan   = 7;
  
  String[] persons = {"Noah", "Ava", "Ryan", "Mia", "Ella",
                      "John", "Lily", "Evan", "Single"};

  int[] spouses = {Ava,  // Noah
                   Noah, // Ava
                   Mia,  // Rya
                   Ryan, // Mia
                   John, // Ella
                   Ella, // John
                   Evan, // Lily
                   Lily, // Evan
                   -1};  // Single has no spouse

  int n;

  IntVar[] santas;
  IntVar[] santa_distance;
  IntVar z; // total "distance" to maximize

  @Override
  public void buildModel() {

    //
    // The matrix version of earlier rounds.
    // M means that no earlier Santa has been assigned.
    // Note: Ryan and Mia has the same recipient for years 3 and 4,
    //       and Ella and John has for year 4.
    //       This seems to be caused by modification of
    //       original data.
    //
    int n_no_single = 8;
    int M = n_no_single + 1;
    int[][] rounds_no_single = {
      // N  A  R  M  El J  L  Ev
      {0, M, 3, M, 1, 4, M, 2}, // Noah
      {M, 0, 4, 2, M, 3, M, 1}, // Ava
      {M, 2, 0, M, 1, M, 3, 4}, // Ryan
      {M, 1, M, 0, 2, M, 3, 4}, // Mia
      {M, 4, M, 3, 0, M, 1, 2}, // Ella
      {1, 4, 3, M, M, 0, 2, M}, // John
      {M, 3, M, 2, 4, 1, 0, M}, // Lily
      {4, M, 3, 1, M, 2, M, 0}  // Evan
    };
    
    //
    // Rounds with a single person (fake data)
    //
    int n_with_single = 9;
    M = n_with_single + 1;
    int[][] rounds_single = {
      // N  A  R  M  El J  L  Ev S
      {0, M, 3, M, 1, 4, M, 2, 2}, // Noah
      {M, 0, 4, 2, M, 3, M, 1, 1}, // Ava
      {M, 2, 0, M, 1, M, 3, 4, 4}, // Ryan
      {M, 1, M, 0, 2, M, 3, 4, 3}, // Mia
      {M, 4, M, 3, 0, M, 1, 2, M}, // Ella
      {1, 4, 3, M, M, 0, 2, M, M}, // John
      {M, 3, M, 2, 4, 1, 0, M, M}, // Lily
      {4, M, 3, 1, M, 2, M, 0, M}, // Evan
      {1, 2, 3, 4, M, 2, M, M, 0}  // Single
    };
  
    n = n_no_single;
    int[][] rounds = rounds_no_single;
    

    if (single) {
      n = n_with_single;
      rounds = rounds_single;
    }
    M = n + 1;

    System.out.println("n:" + n);


    santas = VariableFactory.boundedArray("santas", n, 0, n-1, solver);
    santa_distance = VariableFactory.boundedArray("santa_distance", n, 0, M, solver);
    z = VariableFactory.bounded("z", 0, M*M, solver);


    solver.post(IntConstraintFactory.alldifferent(santas, "BC"));

    // Can't be one own"s Secret Santa.
    // (i.e. ensure that there are no fix-point in the array.)
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.arithm(santas[i],"!=", VariableFactory.fixed(i, solver)));
    }
    
    

    // no Santa for a spouses
    for(int i = 0; i < n; i++) {
      if (spouses[i] > -1) {
        // santas[i] != spouses[i]
        solver.post(IntConstraintFactory.arithm(santas[i], "!=", VariableFactory.fixed(spouses[i], solver)));
      }
    }

    // optimize "distance" to earlier rounds:
    for(int i = 0; i < n; i++) {
      // solver.Add(santa_distance[i] == rounds[i].Element(santas[i]));
      // santa_distance[i] = rounds[i,santas[i]]
      solver.post(IntConstraintFactory.element(santa_distance[i],rounds[i], santas[i], 0, "detect")); 
      
    }


    // cannot be a Secret Santa for the same person
    // two years in a row.
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (rounds[i][j] == 1) {
          solver.post(IntConstraintFactory.arithm(santas[i], "!=", VariableFactory.fixed(j, solver)));
        }
      }
    }

    solver.post(IntConstraintFactory.sum(santa_distance, z));

  }

  @Override
  public void createSolver() {
    solver = new Solver("SecretSanta2");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(santas));
  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, z);
  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("total distance: " + z.getValue());
        System.out.print("santas: ");
        for(int i = 0; i < n; i++) {
          System.out.print(santas[i].getValue()+ " ");
        }
        System.out.println();
        for(int i = 0; i < n; i++) {
          int v = santas[i].getValue();
          System.out.format("%d (%-6s) is a Secret Santa of %d (%-6s) (distance: %d)\n", 
                            i, persons[i], v, persons[v], santa_distance[i].getValue());
        }
        System.out.println();

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

    new SecretSanta2().execute(args);

  }
}

