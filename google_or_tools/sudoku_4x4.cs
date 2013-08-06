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
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Google.OrTools.ConstraintSolver;


public class Sudoku
{

  /**
   *
   * Solves a Sudoku problem.
   *
   * This is a very simple 4x4 problem instance:
   * Problem 26: Shidoku from
   * "Taking Sudoku Seriously", page 61
   *   4 _  _ _
   *   3 1  _ _
   *
   *   _ _  4 1
   *   _ _  _ 2
   *
   */
  private static void Solve()
  {
    Solver solver = new Solver("Sudoku");

    //
    // data
    //
    int block_size = 2;
    IEnumerable<int> BLOCK = Enumerable.Range(0, block_size);
    int n = block_size * block_size;
    IEnumerable<int> RANGE = Enumerable.Range(0, n);

    // 0 marks an unknown value
    int[,] initial_grid = {{4, 0,  0, 0},
                           {3, 1,  0, 0},

                           {0, 0,  4, 1},
                           {0, 0,  0, 2}};
    

    //
    // Decision variables
    //
    IntVar[,] grid =  solver.MakeIntVarMatrix(n, n, 1, n, "grid");
    IntVar[] grid_flat = grid.Flatten();

    //
    // Constraints
    //  

    // init
    foreach(int i in RANGE) {
      foreach(int j in RANGE) {
        if (initial_grid[i,j] > 0) {
          solver.Add(grid[i,j] == initial_grid[i,j]);
        }
      }
    }

    
    foreach(int i in RANGE) {

      // rows
      solver.Add( (from j in RANGE
                   select grid[i,j]).ToArray().AllDifferent());

      // cols
      solver.Add( (from j in RANGE
                   select grid[j,i]).ToArray().AllDifferent());

    }

    // blocks
    foreach(int i in BLOCK) {
      foreach(int j in BLOCK) {
        solver.Add( (from di in BLOCK
                     from dj in BLOCK
                     select grid[i*block_size+di, j*block_size+dj]
                     ).ToArray().AllDifferent());
      }
    }        


    //
    // Search
    //
    DecisionBuilder db = solver.MakePhase(grid_flat,
                                          Solver.INT_VAR_SIMPLE,
                                          Solver.INT_VALUE_SIMPLE);

    solver.NewSearch(db);

    while (solver.NextSolution()) {
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++){ 
          Console.Write("{0} ", grid[i,j].Value());
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
