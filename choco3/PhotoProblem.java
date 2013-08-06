/**
 *
 * Photo problem in Choco3.
 *
 * Problem statement from Mozart/Oz tutorial:
 * http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
 * """
 * Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one
 * row for taking a photo. Some of them have preferences next to whom
 * they want to stand:
 *
 *  1. Betty wants to stand next to Gary and Mary.
 *  2. Chris wants to stand next to Betty and Gary.
 *  3. Fred wants to stand next to Mary and Donald.
 *  4. Paul wants to stand next to Fred and Donald.
 *
 * Obviously, it is impossible to satisfy all preferences. Can you find
 * an alignment that maximizes the number of satisfied preferences?
 * """
 *
 *  Oz solution (the position array, 1-based):
 *     6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
 *  [5, 6, 1, 3, 7, 4, 2]
 *
 *
 *  There are four optimal solutions with 6 fullfilled preferences 
 *  (the positions, in posinv):
 *    [Donald or Paul] Fred Mary Betty [Chris or Gary]
 *
 *  I.e. Donald/Paul are either first/last and Chris and Gary are either 5th/6th.
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class PhotoProblem extends AbstractProblem {
  
  @Option(name = "-show_all_opt", usage = "Show all optimal solutions (default false).", required = false)
  boolean show_all_opt = false;
  
  
  String[] persons = {"Betty", "Chris", "Donald", "Fred", "Gary", "Mary", "Paul"};
  int n = persons.length;
  
  int[][] preferences = { 
    // 0 1 2 3 4 5 6
    // B C D F G M P
    {  0,0,0,0,1,1,0 },  // Betty  0
    {  1,0,0,0,1,0,0 },  // Chris  1
    {  0,0,0,0,0,0,0 },  // Donald 2
    {  0,0,1,0,0,1,0 },  // Fred   3
    {  0,0,0,0,0,0,0 },  // Gary   4
    {  0,0,0,0,0,0,0 },  // Mary   5
    {  0,0,1,1,0,0,0 }}; // Paul   6


  IntVar[] positions;
  IntVar[] posinv;
  IntVar z;


  @Override
  public void buildModel() {

    System.out.println("Preferences:");
    System.out.println("1. Betty wants to stand next to Gary and Mary.");
    System.out.println("2. Chris wants to stand next to Betty and Gary.");
    System.out.println("3. Fred wants to stand next to Mary and Donald.");
    System.out.println("4. Paul wants to stand next to Fred and Donald.\n");

    
    positions = VariableFactory.enumeratedArray("positions", n, 0, n-1, solver);
    posinv = VariableFactory.enumeratedArray("posiv", n, 0, n-1, solver);
    z = VariableFactory.enumerated("z", 0, n*n, solver);

    solver.post(IntConstraintFactory.alldifferent(positions, "BC"));

    //
    // Calculate all the successful preferences
    //
    BoolVar[][] b = VariableFactory.boolMatrix("b", n, n, solver);
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (preferences[i][j] == 1) {
          solver.post(IntConstraintFactory.implies(b[i][j],
                                                 IntConstraintFactory.distance(positions[i],positions[j],"=", 1)));
          solver.post(IntConstraintFactory.implies(VariableFactory.not(b[i][j]),
                                                 IntConstraintFactory.distance(positions[i],positions[j],"!=", 1)));
        } else {
          solver.post(IntConstraintFactory.arithm(b[i][j],"=", 0));
        }
      }

      solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(b),z));

      // inverse channeling for showing the places of each person
      solver.post(IntConstraintFactory.inverse_channeling(positions, posinv, 0, 0));

      if (show_all_opt) {
        solver.post(IntConstraintFactory.arithm(z,"=", 6));
      }
    }

    // Symmetry breaking (from the Oz page):
    //    Fred is somewhere left of Betty
    solver.post(IntConstraintFactory.arithm(positions[3],"<", positions[0]));

  }

  @Override
  public void createSolver() {
    solver = new Solver("PhotoProblem");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(positions));
  }

  @Override
  public void solve() {
    if (show_all_opt) {
      solver.findSolution();
    } else {
      solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, z);
    }
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("z: " + z.getValue());
        System.out.print("position: ");
        for(int i = 0; i < n; i++) {
          System.out.print(positions[i].getValue() + " ");
        }
        System.out.println();
        System.out.print("places: ");
        for(int i = 0; i < n; i++) {
          System.out.print(persons[posinv[i].getValue()] + " ");
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

    new PhotoProblem().execute(args);

  }

}

