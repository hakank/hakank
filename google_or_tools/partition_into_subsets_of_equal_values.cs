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
   * From Programmers Stack Exchange (C#)
   * http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
   * Partitioning set into subsets with respect to equality of sum among subsets
   * """
   * let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
   * numbers such that sum of subset1 should be equal to sum of subset2 
   * {3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
   * way might be dividable by 2 without any remainder) and if we can, we should 
   * write our algorithm two create s1 and s2 out of s.
   *
   * How to proceed with this approach? I read partition problem in wiki and even in some 
   * articles but i am not able to get anything. Can someone help me to find the 
   * right algorithm and its explanation in simple English?
   *
   *
   * Also see http://www.hakank.org/or-tools/set_partition.cs
   *
   * Model by Hakan Kjellerstrand (hakank@gmail.com)
   * See other or-tools/C# models at http://www.hakank.org/or-tools/#csharp
   *
   */
  private static void Solve()
  {

    Solver solver = new Solver("PartitionSets");

    //
    // Data
    //
    int[] s = {3, 1, 1, 2, 2, 1, 5, 2, 7};
    int n = s.Length;
    int num_subsets = 2;

    IEnumerable<int> NRange = Enumerable.Range(0, n);
    IEnumerable<int> Sets = Enumerable.Range(0, num_subsets);

    //
    // Decision variables
    //

    // To which subset do x[i] belong?
    IntVar[,] x = solver.MakeIntVarMatrix(num_subsets, n, 0, 1, "x");
    IntVar[] x_flat = x.Flatten();


    //
    // Constraints
    //

    // Ensure that a number is in exact one subset
    for(int k = 0; k < n; k++) {
      solver.Add( (from p in Sets select (x[p,k])).ToArray().Sum() == 1);
    }

    // Ensure that the sum of all subsets are the same.
    for(int p = 0; p < num_subsets-1; p++) {
        solver.Add(
                   (from k in NRange select (s[k]*x[p,k])).ToArray().Sum()
                   ==
                   (from k in NRange select (s[k]*x[p+1,k])).ToArray().Sum()
                   );
    }


    // symmetry breaking: assign first number to subset 1
    solver.Add(x[0,0] == 1);

    //
    // Search
    //
    DecisionBuilder db = solver.MakePhase(x_flat,
                                          Solver.INT_VAR_DEFAULT,
                                          Solver.INT_VALUE_DEFAULT);

    solver.NewSearch(db);

    while (solver.NextSolution()) {

      foreach(int i in Sets) {
        Console.Write("subset " + i + ": ");
        foreach(int j in NRange) {

          if ((int)x[i,j].Value() == 1) {
            Console.Write(s[j] + " ");
          }
        }
        Console.WriteLine();
      }
      Console.WriteLine();

    }

    Console.WriteLine("\nSolutions: {0}", solver.Solutions());
    Console.WriteLine("WallTime: {0}ms", solver.WallTime());
    Console.WriteLine("Failures: {0}", solver.Failures());
    Console.WriteLine("Branches: {0} ", solver.Branches());

    solver.EndSearch();

  }

  public static void Main(String[] args)
  {
    Solve();
  }
}
