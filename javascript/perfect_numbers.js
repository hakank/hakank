/* 

  Perfect numbers in JavaScript.

   See http://rosettacode.org/wiki/Perfect_numbers
   """
   A number is perfect if the sum of its factors is equal to 
   twice the number. An equivalent condition is that n is perfect 
   if the sum of n's factors that are less than n is equal to n. 
   """

  The first perfect numbers are [6 28 496 8128]


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {sum2,range2,all_divisors3,timing2} = require('./js_utils.js');

//
// Return all numbers below limit which are perfect numbers
//
function perfect_numbers(limit) {
    let p = [];
    let n = 2;
    while(n <= limit) {
        const divisors = all_divisors3(n); // all divisors including 1 but not n
        if (divisors.sum2() == n) {
            p.push(n);
        }
        n++;
    }
    return p;
}

// Functional
function perfect_numbers2(limit) {
    let p = [];
    return range2(2,limit)
        .filter(n=>n === all_divisors3(n).sum2()); 
}

/*
perfect_numbers1_test1
[ 5, [ 6, 28, 496 ] ]
perfect_numbers1_test2
[ 82, [ 6, 28, 496, 8128 ] ]
perfect_numbers1_test3
[ 7711, [ 6, 28, 496, 8128 ] ]

perfect_numbers2_test1
[ 1, [ 6, 28, 496 ] ]
perfect_numbers2_test2
[ 78, [ 6, 28, 496, 8128 ] ]
perfect_numbers2_test3
[ 7858, [ 6, 28, 496, 8128 ] ]
*/
timing2(function perfect_numbers1_test1() { return perfect_numbers(1_000)});
timing2(function perfect_numbers1_test2() { return perfect_numbers(10_000)});
timing2(function perfect_numbers1_test3() { return perfect_numbers(100_000)});
console.log("\n");
timing2(function perfect_numbers2_test1() { return perfect_numbers2(1_000)});
timing2(function perfect_numbers2_test2() { return perfect_numbers2(10_000)});
timing2(function perfect_numbers2_test3() { return perfect_numbers2(100_000)});



    
