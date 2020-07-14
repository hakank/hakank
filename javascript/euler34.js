/* 

  Euler #34 in JavaScript.

  Problem 34
  """
  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
  
  Find the sum of all numbers which are equal to the sum of the 
  factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {factorial2,num_to_list2,sum2,range2,timing2} = require('./js_utils.js');

const str_factorial_sum = function(n) {
    // return n.toString().split("").map(i=>factorial(parseInt(i))).sum2();
    // return n.toString().split("").map(i=>parseInt(i).factorial2()).sum2();
    return [...n.toString()].map(i=>parseInt(i).factorial2()).sum2();        
    // return n.num_to_list2().map(i=>factorial(i)).sum2(); // slower: 84ms
}

// 61ms
const euler34a = function() {
    let s = 0;
    for(let n = 10; n <= 100000; n++) {
        if (n === str_factorial_sum(n)) {
            s+=n;
        }
    }
    return s;        
}

// 96ms
const euler34b = function() {
    return range2(10,100000)
        .filter(n=>n === str_factorial_sum(n))
        .sum2();
}

// timing2(euler34a); // 200ms
timing2(euler34b); // 192ms
