//
// Copyright 2012 Hakan Kjellerstrand (hakank@gmail.com)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;
using Google.OrTools.ConstraintSolver;

public class AppointmentSchedulingSet
{

  /**
   Appointment scheduling (set approach) in or-tools/C#.
   
   From Stack Overflow 
   Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)
   http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
   """
   Problem statement

   We have one employer that wants to interview N people, and therefore makes N 
   interview slots. Every person has a free-busy schedule for those slots. Give an 
   algorithm that schedules the N people into N slots if possible, and return a 
   flag / error / etc if it is impossible. What is the fastest possible runtime complexity?

   My approaches so far

   Naive: there are N! ways to schedule N people. Go through all of them, for each 
   permutation, check if it's feasible. O( n! )

   Backtracking:

   1. Look for any interview time slots that can only have 1 person. Schedule the person, 
      remove them from the list of candidates and remove the slot.
   2. Look for any candidates that can only go into 1 slot. Schedule the person, remove 
      them from the list of candidates and remove the slot.
   3. Repeat 1 & 2 until there are no more combinations like that.
   4. Pick a person, schedule them randomly into one of their available slots. Remember 
      this operation.
   5. Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we 
      have a schedule, return it. If there's an unresolvable conflict, backtrack.

   This is O( n! ) worst case, I think - which isn't any better.

   There might be a D.P. solution as well - but I'm not seeing it yet.

   Other thoughts

   The problem can be represented in an NxN matrix, where the rows are "slots", columns 
   are "people", and the values are "1" for free and "0" for busy. Then, we're looking for 
   a row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for 
   a row or a column with only one "1". (If the rank of the matrix is = N, I that means that 
   there is a solution. But the opposite does not hold) Another way to look at it is to 
   treat the matrix as an unweighed directed graph edge matrix. Then, the nodes each 
   represent 1 candidate and 1 slot. We're then looking for a set of edges so that every 
   node in the graph has one outgoing edge and one incoming edge. Or, with 2x nodes, it would 
   be a bipartite graph.

   Example of a matrix:

     1 1 1 1
     0 1 1 0
     1 0 0 1
     1 0 0 1
   """

   This model implements the set based approach mentioned in my answer to the
   question.
   
   Compare with my MiniZinc model:
   http://www.hakank.org/minizinc/appointment_scheduling_set.mzn


   Model by Hakan Kjellerstrand (hakank@gmail.com)
   See other or-tools/C# models at http://www.hakank.org/or-tools/#csharp
   
  */
  private static void Solve(int n = 10, int sols_to_show = 0, double limit = 0.5d)
  {

    Solver solver = new Solver("AppointmentScheduling");

    Console.WriteLine("n: " + n);
    Console.WriteLine("sols_to_show: " + sols_to_show);
    Console.WriteLine("limit: " + limit);

    //
    // Data
    //

    int printLimit = 100;

    // 
    // Randomize data, i.e. for each time slot randomize if person j is 
    // available
    //
    int seed = (int)DateTime.Now.Ticks;
    Random generator = new Random(seed);

    int[][] s = new int[n][];
    int c = 0;
    // For each time slot, get the people who are available
    for(int i = 0; i < n; i++) {
      HashSet<int> tmp = new HashSet<int>();

      for(int j = 0; j < n; j++) {
        double r = generator.NextDouble();
        if (r >= limit) {
          tmp.Add(j+1);
          c++;
          if (n <= printLimit) {
            Console.Write(j+1 + " ");
          }
        }
      }
      // We need at least one person per time slot.
      if (tmp.Count() == 0) {
        int v = 1+generator.Next(n); 
        if (n <= printLimit) {
          Console.Write(v);
        }
        tmp.Add(v);
        c++;
      }
      if (n <= printLimit) {
        Console.WriteLine();
      }
      s[i] = tmp.ToArray();
    }

    // the original problem
    /*
    int[][] s = new int[][] {
      new int[] {1,2,3,4},
      new int[] {2,3},
      new int[] {1,4},
      new int[] {1,4}
    };
    */

    Console.WriteLine("\nNumber of generated: {0}", c);

    IEnumerable<int> NRange = Enumerable.Range(0, n);

    //
    // Decision variables
    //

    // The assignment of persons to a time slot (appointment number 1..n).
    IntVar[] x = solver.MakeIntVarArray(n, 1, n, "x");

    //
    // Constraints
    //

    // Ensure that each person is alotted to exactly one time slot only.
    solver.Add(x.AllDifferent());

    // Ensure that the selected person for the alotted time is avaiable.
    IntVar[] b = new IntVar[n]; 
    foreach(int i in NRange) {
      b[i] = x[i].IsMember(s[i]).Var();
    }
    solver.Add(b.Sum()==n);


    //
    // Search
    //
    DecisionBuilder db = solver.MakePhase(x,
                                          // Solver.INT_VAR_DEFAULT,
                                          // Solver.INT_VAR_SIMPLE,
                                          // Solver.CHOOSE_FIRST_UNBOUND,
                                          // Solver.CHOOSE_RANDOM,
                                          // Solver.CHOOSE_MIN_SIZE_LOWEST_MIN, // <-
                                          Solver.CHOOSE_MIN_SIZE_HIGHEST_MIN,
                                          // Solver.CHOOSE_MIN_SIZE_LOWEST_MAX,
                                          // Solver.CHOOSE_MIN_SIZE_HIGHEST_MAX,
                                          // Solver.CHOOSE_PATH,


                                          Solver.INT_VALUE_DEFAULT
                                          // Solver.INT_VALUE_SIMPLE
                                          // Solver.ASSIGN_MIN_VALUE
                                          // Solver.ASSIGN_MAX_VALUE
                                          // Solver.ASSIGN_RANDOM_VALUE
                                          // Solver.ASSIGN_CENTER_VALUE
                                          );

    solver.NewSearch(db);

    int sols = 0;
    Console.WriteLine("\nSolution:");
    while (solver.NextSolution()) {
      sols++;
      Console.WriteLine("solution #" + sols);
      foreach(int i in NRange) {
        Console.Write(x[i].Value() + " ");
      }

      Console.WriteLine();

      if (sols_to_show > 0 && sols >= sols_to_show) {
        break;
      }

    }

    Console.WriteLine("\nSolutions: {0}", solver.Solutions());
    Console.WriteLine("WallTime: {0}ms", solver.WallTime());
    Console.WriteLine("Failures: {0}", solver.Failures());
    Console.WriteLine("Branches: {0} ", solver.Branches());
    // Console.WriteLine("MemoryUsage: {0} ", solver.MemoryUsage());

    solver.EndSearch();

  }

  
  /** 
   *  Syntax: picking_teams.exe n max sols_to_show
   * 
   *  - n: number of ints to generate. Default 10.
   *  - sols_to_show: number of solutions to show. Default all (0)
   *
   */
  public static void Main(String[] args)
  {
    int n = 10;
    int sols_to_show = 0;
    double limit = 0.5d;

    if (args.Length > 0) {
      n = Convert.ToInt32(args[0]);
    }

    if (args.Length > 1) {
      sols_to_show = Convert.ToInt32(args[1]);
    }

    if (args.Length > 2) {
      limit = Convert.ToDouble(args[2]);
    }


    Solve(n, sols_to_show, limit);
  }
}
