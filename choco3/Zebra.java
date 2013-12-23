/**
  *
  * Zebra problem in Choco3.
  *
  * """
  * This is the zebra problem as invented by Lewis Caroll.
  *
  * There are five houses.
  * The Englishman lives in the red house.
  * The Spaniard owns the dog.
  * Coffee is drunk in the green house.
  * The Ukrainian drinks tea.
  * The green house is immediately to the right of the ivory house.
  * The Old Gold smoker owns snails.
  * Kools are smoked in the yellow house.
  * Milk is drunk in the middle house.
  * The Norwegian lives in the first house.
  * The man who smokes Chesterfields lives in the house next to the man
  *   with the fox.
  * Kools are smoked in the house next to the house where the horse is kept.
  * The Lucky Strike smoker drinks orange juice.
  * The Japanese smokes Parliaments.
  * The Norwegian lives next to the blue house.
  *
  * Who owns a zebra and who drinks water?
  * """
  * 
  * This Choco3 model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my Choco page: http://www.hakank.org/choco/ 
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
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Zebra extends AbstractProblem {

  int n = 5;

  IntVar[] colors;
  IntVar[] nationality;
  IntVar[] animal;
  IntVar[] drink;
  IntVar[] smoke;
  IntVar[] all;

  @Override
  public void createSolver() {
    solver = new Solver("Zebra");
  }
  

  @Override
  public void buildModel() {    

    // Colors
    IntVar red = VariableFactory.enumerated("red", 1, n, solver);
    IntVar green = VariableFactory.enumerated("green", 1, n, solver);
    IntVar yellow = VariableFactory.enumerated("yellow", 1, n, solver);
    IntVar blue = VariableFactory.enumerated("blue", 1, n, solver);
    IntVar ivory = VariableFactory.enumerated("ivory", 1, n, solver);
    colors = new IntVar[] {red, green, yellow, blue, ivory};

    // Nationality
    IntVar englishman = VariableFactory.enumerated("englishman", 1, n, solver);
    IntVar spaniard = VariableFactory.enumerated("spaniard", 1, n, solver);
    IntVar japanese = VariableFactory.enumerated("japanese", 1, n, solver);
    IntVar ukranian = VariableFactory.enumerated("ukranian", 1, n, solver);    
    IntVar norwegian = VariableFactory.enumerated("norwegian", 1, n, solver);
    nationality = new IntVar[] {englishman, spaniard, japanese, ukranian, norwegian};
    
    // Animal
    IntVar dog = VariableFactory.enumerated("dog", 1, n, solver);
    IntVar snails = VariableFactory.enumerated("snails", 1, n, solver);
    IntVar fox = VariableFactory.enumerated("fox", 1, n, solver);
    IntVar zebra = VariableFactory.enumerated("zebra", 1, n, solver);    
    IntVar horse = VariableFactory.enumerated("horse", 1, n, solver);
    animal = new IntVar[] {dog, snails, fox, zebra, horse};
    
    // Drink
    IntVar tea = VariableFactory.enumerated("tea", 1, n, solver);
    IntVar coffee = VariableFactory.enumerated("coffee", 1, n, solver);
    IntVar water = VariableFactory.enumerated("water", 1, n, solver);
    IntVar milk = VariableFactory.enumerated("milk", 1, n, solver);    
    IntVar fruit_juice = VariableFactory.enumerated("fruit_juice", 1, n, solver);
    drink = new IntVar[] {tea, coffee, water, milk, fruit_juice};
    
    // Smoke
    IntVar old_gold = VariableFactory.enumerated("old_gold", 1, n, solver);
    IntVar kools = VariableFactory.enumerated("kools", 1, n, solver);
    IntVar chesterfields = VariableFactory.enumerated("chesterfields", 1, n, solver);
    IntVar lucky_strike = VariableFactory.enumerated("lucky_strike", 1, n, solver);    
    IntVar parliaments = VariableFactory.enumerated("parliaments", 1, n, solver);
    smoke = new IntVar[] {old_gold, kools, chesterfields, lucky_strike, parliaments};


    solver.post(IntConstraintFactory.alldifferent(colors, "BC"));
    solver.post(IntConstraintFactory.alldifferent(nationality, "BC"));
    solver.post(IntConstraintFactory.alldifferent(animal, "BC"));
    solver.post(IntConstraintFactory.alldifferent(drink, "BC"));
    solver.post(IntConstraintFactory.alldifferent(smoke, "BC"));

    all = new IntVar[n*5];
    all = ArrayUtils.append(colors, nationality, animal, drink, smoke);

    //
    // The clues
    //
    solver.post(IntConstraintFactory.arithm(englishman,"=",red));
    solver.post(IntConstraintFactory.arithm(spaniard,"=",dog));
    solver.post(IntConstraintFactory.arithm(coffee,"=",green));
    solver.post(IntConstraintFactory.arithm(ukranian,"=",tea));
    solver.post(IntConstraintFactory.arithm(green,"=",VariableFactory.offset(ivory,1)));
    solver.post(IntConstraintFactory.arithm(old_gold,"=",snails));
    solver.post(IntConstraintFactory.arithm(kools,"=",yellow));
    solver.post(IntConstraintFactory.arithm(milk,"=",VariableFactory.fixed(3,solver)));
    solver.post(IntConstraintFactory.arithm(norwegian,"=",VariableFactory.fixed(1,solver)));
    solver.post(IntConstraintFactory.distance(fox, chesterfields,"=",1));
    solver.post(IntConstraintFactory.distance(horse, kools,"=",1));
    solver.post(IntConstraintFactory.arithm(lucky_strike,"=",fruit_juice));
    solver.post(IntConstraintFactory.arithm(japanese,"=",parliaments));
    solver.post(IntConstraintFactory.distance(norwegian, blue,"=",1));


  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(all));
  }
  
  @Override
  public void solve() {
    solver.findSolution();
  }

  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.print("Colors      : ");
        for (int i = 0; i < n; i++) {
          System.out.print(colors[i].getValue() + " ");
        }
        System.out.print("\nNationality : ");
        for (int i = 0; i < n; i++) {
          System.out.print(nationality[i].getValue() + " ");
        }
        System.out.print("\nAnimal      : ");
        for (int i = 0; i < n; i++) {
          System.out.print(animal[i].getValue() + " ");
        }
        System.out.print("\nDrink       : ");
        for (int i = 0; i < n; i++) {
          System.out.print(drink[i].getValue() + " ");
        }
        System.out.print("\nSmoke       : ");
        for (int i = 0; i < n; i++) {
          System.out.print(smoke[i].getValue() + " ");
        }
        System.out.println();

        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {

    new Zebra().execute(args);

  }


}
 
