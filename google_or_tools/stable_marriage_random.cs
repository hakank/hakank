//
// Copyright 2012 Hakan Kjellerstrand
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
using System.Collections;
using System.IO;
using System.Text.RegularExpressions;
using Google.OrTools.ConstraintSolver;

public class StableMarriageRandom
{

  // generates a shuffled array of size n
  private static int[] shuffle(int n, Random generator) {
    int[] x = new int[n];
    for (int i = 0; i < n; i++) {
      x[i] = i;
    }
    for (int i = x.Length - 1; i > 0; i--)
    {
      int nn = generator.Next(i + 1);
      int t = x[i];
      x[i] = x[nn];
      x[nn] = t;
    }

    return(x);
  }

  // print the rank matrix
  private static void printRank(string s, int[][] rank) {
    Console.WriteLine(s + ":");
    int n = rank.Length;
    for (int i = 0; i < n; i++) {
      Console.Write("r" + i + ": ");
      for (int j = 0; j < n; j++) {
        Console.Write(rank[i][j] + " ");
      }
      Console.WriteLine();
    } 
    Console.WriteLine();

  }

  /**
   *
   * Solves some stable marriage problems.
   *
   * This version randomize the problem instances.
   *
   * Also, see http://www.hakank.org/or-tools/stable_marriage.cs
   *
   *
   */
  private static void Solve(int n = 10, int sols_to_show = 0)
  {

    Solver solver = new Solver("StableMarriage");

    //
    // data
    //
    int seed = (int)DateTime.Now.Ticks;
    Random generator = new Random(seed);

    int[][] rankMen = new int[n][];
    int[][] rankWomen = new int[n][];
    for (int i = 0; i < n; i++) {
      int[] m = shuffle(n, generator); 
      int[] w = shuffle(n, generator); 
      rankMen[i] = new int[n];
      rankWomen[i] = new int[n];
      for (int j = 0; j < n; j++) {
        rankMen[i][j] = m[j];
        rankWomen[i][j] = w[j];
      }
    } 

    Console.WriteLine("after generating...");

    if (n <= 20) {
      Console.Write("rankMen: ");

      printRank("rankMen", rankMen);
      printRank("rankWomen", rankWomen);

      
    }

    //
    // Decision variables
    //
    IntVar[] wife = solver.MakeIntVarArray(n, 0, n - 1, "wife");
    IntVar[] husband = solver.MakeIntVarArray(n, 0, n - 1, "husband");
    

    //
    // Constraints
    //
    // (The comments below are the Comet code)

    //
    //   forall(m in Men)
    //      cp.post(husband[wife[m]] == m);
    for(int m = 0; m < n; m++) {
      solver.Add(husband.Element(wife[m]) == m);
    }

    //   forall(w in Women)
    //     cp.post(wife[husband[w]] == w);
    for(int w = 0; w < n; w++) {
      solver.Add(wife.Element(husband[w]) == w);
    }


    //   forall(m in Men, o in Women)
    //       cp.post(rankMen[m,o] < rankMen[m, wife[m]] =>
    //               rankWomen[o,husband[o]] < rankWomen[o,m]);
    for(int m = 0; m < n; m++) {
      for(int o = 0; o < n; o++) {
        IntVar b1 = rankMen[m].Element(wife[m]) > rankMen[m][o];
        IntVar b2 = rankWomen[o].Element(husband[o]) < rankWomen[o][m];
        solver.Add(b1 <= b2);
      }

    }

    //   forall(w in Women, o in Men)
    //      cp.post(rankWomen[w,o] < rankWomen[w,husband[w]] =>
    //              rankMen[o,wife[o]] < rankMen[o,w]);
    for(int w = 0; w < n; w++) {
      for(int o = 0; o < n; o++) {
        IntVar b1 = rankWomen[w].Element(husband[w]) > rankWomen[w][o];
        IntVar b2 = rankMen[o].Element(wife[o]) < rankMen[o][w];
        solver.Add(b1 <= b2);
        }
      }


    //
    // Search
    //
    DecisionBuilder db = solver.MakePhase(wife,
                                          // Solver.INT_VAR_DEFAULT,
                                          // Solver.INT_VAR_SIMPLE,
                                          Solver.CHOOSE_FIRST_UNBOUND,
                                          // Solver.CHOOSE_RANDOM,
                                          // Solver.CHOOSE_MIN_SIZE_LOWEST_MIN,
                                          // Solver.CHOOSE_MIN_SIZE_HIGHEST_MIN,
                                          // Solver.CHOOSE_MIN_SIZE_LOWEST_MAX,
                                          // Solver.CHOOSE_MIN_SIZE_HIGHEST_MAX,
                                          // Solver.CHOOSE_PATH,
                                          // Solver.CHOOSE_MIN_SIZE,
                                          // Solver.CHOOSE_MAX_SIZE,
                                          // Solver.CHOOSE_MAX_REGRET,
                                        


                                          // Solver.INT_VALUE_DEFAULT
                                          // Solver.INT_VALUE_SIMPLE
                                          Solver.ASSIGN_MIN_VALUE
                                          // Solver.ASSIGN_MAX_VALUE
                                          // Solver.ASSIGN_RANDOM_VALUE
                                          // Solver.ASSIGN_CENTER_VALUE
                                          // Solver.SPLIT_LOWER_HALF
                                          // Solver.SPLIT_UPPER_HALF
                                          );

    solver.NewSearch(db);

    int sols = 0;
    while (solver.NextSolution()) {
      sols += 1;
      Console.Write("wife   : ");
      for(int i = 0; i < n; i++) {
        Console.Write(wife[i].Value() + " ");
      }
      Console.Write("\nhusband: ");
      for(int i = 0; i < n; i++) {
        Console.Write(husband[i].Value() + " ");
      }
      Console.WriteLine("\n");

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



  public static void Main(String[] args)
  {

    int n = 10;
    int sols_to_show = 0;

    if (args.Length > 0) {
      n = Convert.ToInt32(args[0]);
    }

    if (args.Length > 1) {
      sols_to_show = Convert.ToInt32(args[1]);
    }

    Solve(n, sols_to_show);
  }
}
