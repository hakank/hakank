/* 

  Solve the no connection puzzle in JavaScript.

  http://rosettacode.org/wiki/Solve_the_no_connection_puzzle
  """
  You are given a box with eight holes labelled A-to-H, connected by 
  fifteen straight lines in the pattern as shown below:

             A   B
            /│\ /│\
           / │ X │ \
          /  │/ \│  \
         C ─ D ─ E ─ F
          \  │\ /│  /
           \ │ X │ /
            \│/ \│/
             G   H

  You are also given eight pegs numbered   1-to-8.


  Objective

  Place the eight pegs in the holes so that the (absolute) difference between any 
  two numbers connected by any line is greater than one.


  Example

  In this attempt:

             4   7
            /│\ /│\
           / │ X │ \
          /  │/ \│  \
         8 ─ 1 ─ 6 ─ 2
          \  │\ /│  /
           \ │ X │ /
            \│/ \│/
             3   5

  Note that 7 and 6 are connected and have a difference of 1, so it is 
  not a solution.

  Task

  Produce and show here _one_ solution to the puzzle. 
  """

  There are 16 solutions, 8 with symmetry breaking (p[0] < p[7]).
  Here are the 8 solutions with symmetry breaking:

  3 4 7 1 8 2 5 6
  3 5 7 1 8 2 4 6
  3 6 7 1 8 2 4 5
  3 6 7 1 8 2 5 4
  4 3 2 8 1 7 6 5
  4 5 7 1 8 2 3 6
  4 6 7 1 8 2 3 5
  5 4 7 1 8 2 3 6


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2,next_permutation} = require('./js_utils.js');

function no_connection(num_sols=0) {
    const graph =[[0, 2], [0, 3], [0, 4],
                  [1, 3], [1, 4], [1, 5],
                  [2, 0], [2, 3], [2, 6],
                  [3, 0], [3, 1], [3, 2], [3, 4], [3, 6], [3, 7],
                  [4, 0], [4, 1], [4, 3], [4, 5], [4, 6], [4, 7],
                  [5, 1], [5, 4], [5, 7],
                  [6, 2], [6, 3], [6, 4],
                  [7, 3], [7, 4], [7, 5]];
   
    let p = range2(1,8);
    let count = 0;
    while (p !== null) {
        if (graph.every(g=>
                        p[0] < p[7] && // symmetry breaking
                        Math.abs(p[g[0]]-p[g[1]]) > 1
                       )) {
            console.log(p.join(" "));
            count ++;
            // Convert to chars for easier identification.
            const [a,b,c,d,e,f,g,h] = p;
            console.log(
 `           ${a}   ${b}       
          /|\\ /|\\      
         / | X | \\      
        /  |/ \\|  \\    
       ${c} - ${d} - ${e} - ${f} 
        \\  |\\ /|  /    
         \\ | X | /      
          \\|/ \\|/      
           ${g}   ${h}\n`);

            if (num_sols > 0 && count >= num_sols) {
                break;
            }
            

        }
        p = next_permutation(p);
    }
    console.log(`Number of solutions: ${count}`);
    

}


console.log("First solution:");
no_connection(1);

console.log("\nAll solutions:");
no_connection(0);
