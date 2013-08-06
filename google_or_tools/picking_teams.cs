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

public class SetPartition
{

  /**
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
   * Model by Hakan Kjellerstrand (hakank@gmail.com)
   * See other or-tools/C# models at http://www.hakank.org/or-tools/#csharp
   *
   */
  private static void Solve(int n = 10, int max = 10, int sols_to_show = 0)
  {

    Solver solver = new Solver("PickingTeams");

    Console.WriteLine("n: " + n);
    Console.WriteLine("max: " + max);
    Console.WriteLine("sols_to_show: " + sols_to_show);

    if (n % 2 != 0) {
      Console.WriteLine("The number of people (n) must be divisible by 2.");
      System.Environment.Exit(1);
    }

    //
    // Data
    //

    int num_teams = 2;

    // Randomize data:
    int seed = (int)DateTime.Now.Ticks;
    Random generator = new Random(seed);
    int[] s = new int[n];
    for(int i = 0; i < n; i++) {
      s[i] = 1 + generator.Next(max);
      if (n <= 100) {
        Console.Write(s[i] + " ");
      }
    }
    Console.WriteLine();
    Console.WriteLine("\n" + n + " numbers generated\n");

    // int[] s = {35, 52, 17, 26, 90, 55, 57, 54, 41, 9, 75, 24, 17, 23, 62, 74, 100, 67, 40, 48, 7, 6, 44, 19, 16, 14, 2, 66, 70, 2, 43, 45, 76, 53, 90, 12, 88, 96, 30, 30, 36, 93, 74, 1, 52, 45, 38, 7, 24, 96, 17, 21, 12, 12, 23, 90, 77, 64, 37, 79, 67, 62, 24, 11, 74, 82, 51, 17, 72, 18, 37, 94, 43, 44, 32, 86, 94, 33, 97, 27, 38, 38, 29, 92, 35, 82, 22, 66, 80, 8, 62, 72, 25, 13, 94, 42, 51, 31, 69, 66};
    // n = s.Length;

    int the_sum = s.Sum();
    int half_sum = (int)the_sum / 2;
    Console.WriteLine("sum: " + the_sum + " half_sum: " + half_sum);


    IEnumerable<int> NRange = Enumerable.Range(0, n);
    IEnumerable<int> Sets = Enumerable.Range(0, num_teams);

    //
    // Decision variables
    //

    // To which team (s) do x[i] belong?
    IntVar[] x = solver.MakeIntVarArray(n, 0, 1, "x");

    IntVar diff = (
             (from k in NRange select (s[k]*(x[k] == 0)) ).ToArray().Sum() -
             (from k in NRange select (s[k]*(x[k] == 1)) ).ToArray().Sum()
             ).Abs().Var();
      
    Console.WriteLine("diff.Max(): " + diff.Max());


    //
    // Constraints
    //

    // Ensure that there are the same number of people in each team
    int n2 = (int)n / 2;
    solver.Add((from k in NRange select (x[k]==0)).ToArray().Sum() == n2 );
    // solver.Add((from k in NRange select (x[k]==1)).ToArray().Sum() == n2 );

    // symmetry breaking: assign first number to team 0
    solver.Add(x[0] == 0);

    // Odd sum must yield odd diff, even sum yield even diff
    IntVar even_odd = solver.MakeIntConst(the_sum % 2);
    solver.Add(solver.MakeModuloConstraint(diff, 2, even_odd));


    //
    // Search
    //
    DecisionBuilder db = solver.MakePhase(x,
                                          // Solver.INT_VAR_DEFAULT,
                                          // Solver.INT_VAR_SIMPLE,
                                          // Solver.CHOOSE_FIRST_UNBOUND,
                                          // Solver.CHOOSE_RANDOM,
                                          Solver.CHOOSE_MIN_SIZE_LOWEST_MIN,
                                          // Solver.CHOOSE_MIN_SIZE_HIGHEST_MIN,
                                          // Solver.CHOOSE_MIN_SIZE_LOWEST_MAX,
                                          // Solver.CHOOSE_MIN_SIZE_HIGHEST_MAX,
                                          // Solver.CHOOSE_PATH,


                                          // Solver.INT_VALUE_DEFAULT
                                          // Solver.INT_VALUE_SIMPLE
                                          // Solver.ASSIGN_MIN_VALUE
                                          // Solver.ASSIGN_MAX_VALUE
                                          Solver.ASSIGN_RANDOM_VALUE
                                          // Solver.ASSIGN_CENTER_VALUE
                                          );

    // DefaultPhaseParameters parameters = new DefaultPhaseParameters();
    
    // // parameters.heuristic_period = 20000;

    // // parameters.heuristic_num_failures_limit = 1000;
    // // parameters.heuristic_period = 20;
    // // parameters.heuristic_period = 20;
  
    // // parameters.restart_log_size = -1;
    // // parameters.restart_log_size = 1000;
    // // parameters.run_all_heuristics = false;

    // // parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_SUM_IMPACT;
    // // parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_AVERAGE_IMPACT ;
    // // parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_VALUE_IMPACT;
    
    // // parameters.value_selection_schema = DefaultPhaseParameters.SELECT_MIN_IMPACT;
    // // parameters.value_selection_schema = DefaultPhaseParameters.SELECT_MAX_IMPACT;
    
    // // parameters.initialization_splits = 10;
    // // parameters.initialization_splits = 20;
    // // parameters.initialization_splits = n;
  
    // // parameters.random_seed = 0;


    // // DecisionBuilder db = solver.MakeDefaultPhase(x_flat, parameters);
    // DecisionBuilder db = solver.MakeDefaultPhase(x, parameters);

    OptimizeVar opt = diff.Minimize(1);

    solver.NewSearch(db, opt);

    int sols = 0;
    while (solver.NextSolution()) {
      sols++;

      Console.WriteLine("Diff: " + diff.Value());

      if (n <= 100) {
        foreach(int i in NRange) {
          Console.Write(x[i].Value() + " ");
        }
        Console.WriteLine();
      }

      if (sols_to_show > 0 && sols >= sols_to_show) {
        break;
      }


    }

    Console.WriteLine("\nSolutions: {0}", solver.Solutions());
    Console.WriteLine("WallTime: {0}ms", solver.WallTime());
    Console.WriteLine("Failures: {0}", solver.Failures());
    Console.WriteLine("Branches: {0} ", solver.Branches());

    solver.EndSearch();

  }

  
  /** 
   *  Syntax: picking_teams.exe n max sols_to_show
   * 
   *  - n: number of ints to generate. Default 10.
   *  - max: the range of the generated integers (1..max). Default 10.
   *  - sols_to_show: number of solutions to show. Default all (0)
   *
   */
  public static void Main(String[] args)
  {
    int n = 10;
    int max = 10;
    int sols_to_show = 0;

    if (args.Length > 0) {
      n = Convert.ToInt32(args[0]);
    }

    if (args.Length > 1) {
      max = Convert.ToInt32(args[1]);
    }

    if (args.Length > 2) {
      sols_to_show = Convert.ToInt32(args[2]);
    }


    Solve(n, max, sols_to_show);
  }
}
