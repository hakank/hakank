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
   * This version has two changes compared to http://www.hakank.org/or-tools/picking_teams.cs
   * - there can be more than 2 teams
   * - it don't have to be exactly the same number of members in each team
   *
   *
   * Model by Hakan Kjellerstrand (hakank@gmail.com)
   *
   * See other or-tools/C# models at http://www.hakank.org/or-tools/#csharp
   *
   */
  private static void Solve(int n = 10, int num_teams = 2, int team_diff = 1, int max = 10, int sols_to_show = 0)
  {

    Solver solver = new Solver("PickingTeams");

    Console.WriteLine("n        : " + n);
    Console.WriteLine("num_teams: " + num_teams); // New
    Console.WriteLine("team_diff: " + team_diff); // New
    Console.WriteLine("max      : " + max);
    Console.WriteLine("sols_to_show: " + sols_to_show);

    // New: We skip this check
    // if (n % num_teams != 0) {
    //   Console.WriteLine("The number of people (n) must be divisible by 2.");
    //   System.Environment.Exit(1);
    // }

    //
    // Data
    //

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

    int the_sum = s.Sum();
    int half_sum = (int)the_sum / num_teams;
    Console.WriteLine("sum: " + the_sum + " half_sum: " + half_sum);


    IEnumerable<int> NRange = Enumerable.Range(0, n);

    //
    // Decision variables
    //

    // To which team (s) do x[i] belong?
    // New: Added num_teams-1
    IntVar[] x = solver.MakeIntVarArray(n, 0, num_teams-1, "x");

    // The old version for 2 teams
    // IntVar diff = (
    //          (from k in NRange select (s[k]*(x[k] == 0)) ).ToArray().Sum() -
    //          (from k in NRange select (s[k]*(x[k] == 1)) ).ToArray().Sum()
    //          ).Abs().Var();

    // New:
    // Calculate the team sums
    // Note: It is more efficient to restrict lower and upper bounds of the team sums. 
    //       Here I arbitrarily allow a slack of
    //          sum +/- (half_sum/num_teams)
    //       For specific applications this might be adjusted. 
    int n3 = half_sum/num_teams;
    Console.WriteLine("n3: " + n3);
    Console.WriteLine("half_sum-n3 " + (half_sum-n3) + "... half_sum+n3: "  + (half_sum+n3));
    IntVar[] team_sum = solver.MakeIntVarArray(num_teams, half_sum-n3, half_sum+n3, "team_sum");
    for(int team = 0; team < num_teams; team++) {
      team_sum[team] = (from k in NRange select (s[k]*(x[k] == team)) ).ToArray().Sum().Var();
    }

    // New:
    // Calculate the total number of difference points between the
    // teams (to be minimized)
    IntVar diff =  (from t1 in Enumerable.Range(0, num_teams)
                    from t2 in Enumerable.Range(0, num_teams)
                    where t1 < t2
                    select (team_sum[t1]-team_sum[t2]).Abs().Var()).ToArray().Sum().Var();
      
    Console.WriteLine("diff.Max(): " + diff.Max());

    //
    // New: Number of members in each team
    //
    // Note that we restrict the possible values to +/- team_diff.
    int n2 = (int)n / num_teams;
    Console.WriteLine("n2: " + n2);
    IntVar[] team_num = solver.MakeIntVarArray(num_teams, n2-team_diff, n2+team_diff, "team_num");
    for(int team = 0; team < num_teams; team++) {
      team_num[team] = (from k in NRange select (x[k] == team) ).ToArray().Sum().Var();
      
    }

    // New: We send all the IntVar arrays to the solver
    IntVar[] all = x.Concat(team_sum).Concat(team_num).ToArray();


    //
    // Constraints
    //

    // New: Ensure that there are the (about) same number of people in each team.
    for(int t = 0; t < num_teams; t++) {
      solver.Add((((from k in NRange select (x[k]==t)).ToArray().Sum()-n2)).Abs() <= team_diff);
    }

    // The teams_sums should add up to the total sum
    solver.Add(team_sum.Sum()  == the_sum);

    // symmetry breaking: assign first member to team 0
    solver.Add(x[0] == 0);

    // Odd sum must yield odd diff, even sum yield even diff
    IntVar even_odd = solver.MakeIntConst(the_sum % 2);
    solver.Add(solver.MakeModuloConstraint(diff, 2, even_odd));


    //
    // Search
    //

    DecisionBuilder db = solver.MakePhase(all,
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


    /*
    DefaultPhaseParameters parameters = new DefaultPhaseParameters();
    
    parameters.heuristic_period = 20;
    // parameters.heuristic_period = 10;

    // parameters.heuristic_num_failures_limit = 1000; 
    // parameters.restart_log_size = -1;
    // parameters.restart_log_size = 100;
    // parameters.run_all_heuristics = false;

    // parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_SUM_IMPACT;
    // parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_AVERAGE_IMPACT ;
    parameters.var_selection_schema = DefaultPhaseParameters.CHOOSE_MAX_VALUE_IMPACT;
    
    // parameters.value_selection_schema = DefaultPhaseParameters.SELECT_MIN_IMPACT;
    parameters.value_selection_schema = DefaultPhaseParameters.SELECT_MAX_IMPACT;
    
    parameters.initialization_splits = 10;
    // parameters.initialization_splits = 20;
    // parameters.initialization_splits = n;
  
    // parameters.random_seed = 0;

    DecisionBuilder db = solver.MakeDefaultPhase(all, parameters);
    */


    OptimizeVar opt = diff.Minimize(1);

    solver.NewSearch(db, opt);

    int sols = 0;
    while (solver.NextSolution()) {
      sols++;

      Console.WriteLine("\n\nDiff: " + diff.Value());

      if (n <= 10000) {
        Console.WriteLine("Assignment: ");
        long[] assignments = new long[n];
        foreach(int i in NRange) {
          Console.Write(x[i].Value() + " ");
          assignments[i] = x[i].Value();
        }
        Console.WriteLine();

        
        for(int team = 0; team < num_teams; team++) {
          Console.Write("team " + team + ": ");
          foreach(int i in NRange) {
            if (assignments[i] == team) {
              Console.Write((i) + " ");
            }
          }
          Console.WriteLine();
        }

      }

      Console.Write("Sum of each team            : ");
      for(int t = 0; t < num_teams; t++) {
        Console.Write(team_sum[t].Value() + " ");
      }
      Console.WriteLine();

      Console.Write("Number of members in each team: ");
      for(int t = 0; t < num_teams; t++) {
        Console.Write(team_num[t].Value() + " ");
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
    int num_teams = 2; // New
    int team_diff = 2; // New
    int max = 10;
    int sols_to_show = 0;

    if (args.Length > 0) {
      n = Convert.ToInt32(args[0]);
    }


    if (args.Length > 1) {
      num_teams = Convert.ToInt32(args[1]);
    }

    if (args.Length > 2) {
      team_diff = Convert.ToInt32(args[2]);
    }


    if (args.Length > 3) {
      max = Convert.ToInt32(args[3]);
    }

    if (args.Length > 4) {
      sols_to_show = Convert.ToInt32(args[4]);
    }


    Solve(n, num_teams, team_diff, max, sols_to_show);
  }
}
