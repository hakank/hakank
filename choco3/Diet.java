/**
 *
 * Diet problem in choco 3.
 * 
 * Problem from http://www.mcs.vuw.ac.nz/courses/OPRE251/2006T1/Labs/lab09.pdf
 * """
 *  My diet requires that all the food I eat come from one of the four .basic 
 *  food groups. (chocolate cake, ice cream, soft drink, and cheesecake). 
 *  Each (large) slice of chocolate cake costs 50c, 
 *  each scoop of chocolate ice cream costs 20c, 
 *  each bottle of cola costs 30c, 
 *  and each piece of pineapple cheesecake costs 80c. 
 *
 *  Each day, I must ingest at least 500 calories, 
 *  6 oz of chocolate, 
 *  10 oz of sugar, 
 *  and 8 oz of fat.
 *  The nutritional content per unit of each food is shown in the table below. 
 * 
 *  Formulate a linear programming model that can be used to satisfy my daily 
 *  nutritional requirement at minimum cost.

 *  Type of                        Calories   Chocolate    Sugar    Fat
 *  Food                                      (ounces)     (ounces) (ounces)
 *  Chocolate Cake (1 slice)       400           3            2      2
 *  Chocolate ice cream (1 scoop)  200           2            2      4
 *  Cola (1 bottle)                150           0            4      1
 *  Pineapple cheesecake (1 piece) 500           0            4      5
 *
 * """  
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * Also see http://www.hakank.org/choco3/
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
import util.ESat;

import java.util.*;

public class Diet extends AbstractProblem {

  // Show all optimal solutions. 
  // Well, it's not very interesting, since there is just one optimal solution...
  @Option(name = "-showAllOpt", usage = "Show all optimal solutions.", required = false)
  boolean showAllOpt = false;

  ArrayList<IntVar> allVars;
  IntVar[] x;
  IntVar cost;

  int n; // number of ingredients
  int m; // number of food types

  String[] food = {"Chocolate Cake", "Chocolate ice cream", "Cola", "Pineapple cheesecake"};
  String[] ingredients = {"Calories", "Chocolate", "Sugar", "Fat"};
    

  @Override
  public void createSolver() {
    solver = new Solver("Diet");
  }
    

  @Override
  public void buildModel() {    

    n = 4; // number of ingredients
    m = 4; // number of food types

    int[] maxVals = {200000, 2000000, 2000000, 200000};
    int[] price   = {50, 20, 30, 80}; // in cents
    int[] limits  = {500, 6, 10, 8};  // minimum required for a diet

    // Food: 0   1     2    3
    int[][] matrix = {{400, 200, 150, 500},  // calories
                      {  3,   2,   0,   0},  // chocolate
                      {  2,   2,   4,   4},  // sugar
                      {  2,   4,   1,   5}}; // fat


    // create x before using it in SumWeight
    x = new IntVar[m];
    x = VariableFactory.enumeratedArray("x", m, 0, 10, solver);

    IntVar[] sums = VariableFactory.enumeratedArray("sums", n, 0, 10000, solver);
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.scalar(x, matrix[i], sums[i]));
      solver.post(IntConstraintFactory.arithm(sums[i],">=", limits[i]));
    }

    // Objective to minimize: x * price
    cost = VariableFactory.bounded("cost", 0, 10000, solver);
    solver.post(IntConstraintFactory.scalar(x, price, cost));

    if (showAllOpt) {
      // generating all optimal solutions
      solver.post(IntConstraintFactory.arithm(cost, "=", 90)); 
    }


  } // end model


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }

  @Override
  public void solve() {
    System.out.println(solver); // Solver/model before solve.
    if (showAllOpt) {
      solver.findSolution();
    } else {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, cost);
    }

  }

  @Override
  public void prettyOut() {

    LoggerFactory.getLogger("bench").info("Diet");
    StringBuilder st = new StringBuilder();

    if (showAllOpt) {
      st.append("\nShow all optimal solutions.\n");
      solver.findSolution();
      if (solver.isFeasible() == ESat.TRUE) {
        do {
          // output solution
          st.append("\nCost: " + cost.getValue() + "\n");
          for(int i = 0; i < m; i++) {
            st.append(food[i] + ": " + x[i].getValue() + "\n");
          }
          LoggerFactory.getLogger("bench").info(st.toString());
          
        } while (solver.nextSolution() == Boolean.TRUE);
      }
    } else {
      st.append("\nCost: " + cost.getValue() + "\n");
      for(int i = 0; i < m; i++) {
        st.append(food[i] + ": " + x[i].getValue() + "\n");
      }
      LoggerFactory.getLogger("bench").info(st.toString());    
      
    }

    // Solver/model after search.
    System.out.println(solver);

  } // end search


  public static void main(String args[]) {

    new Diet().execute(args);

  } // end main


} // end class Diet

