/* 

  Permutations in JavaScript.

  http://rosettacode.org/wiki/Permutations
  """
  Task

  Write a program that generates all permutations of n different objects.
  (Practically numerals!) 
  """

  See js_utils.js for the implementations.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {next_permutation,all_permutations,range,range2,timing,timing2} = require('./js_utils.js');

const a = range2(1,4);
console.log("using all_permutations:");
console.log(all_permutations(a));

console.log("\nusing next_permutations:");
function test_next_permutation(p,print) {
    while (p !== null) {
        if (print) {
            console.log(p);
        }
        p = next_permutation(p);
    }
}

let p = a;
test_next_permutation(p,true);

// Test the speed of the two approaches
const b = range(9);
console.log("using all_permutations (ms):");
console.log(timing(() => {return all_permutations(b);}));

console.log("\nusing next_partition (ms):");
console.log(timing(() => { test_next_permutation(b,false)}));


        
