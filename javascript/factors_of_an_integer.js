/* 

  Factors of an integer in JavaScript.

  http://rosettacode.org/wiki/Factors_of_an_integer
  """
  Task
  
  Compute the factors of a positive integer.

  These factors are the positive integers by which the number being factored 
  can be divided to yield a positive integer result.

  (Though the concepts function correctly for zero and negative integers, the 
  set of factors of zero has countably infinite members, and the factors of 
  negative integers can be obtained from the factors of related positive 
  numbers without difficulty; this task does not require handling of either 
  of these cases).

  Note that every prime number has two factors: 1 and itself. 
  """

  Note: The definition of "factors" varies a bit. Here we use the
  all_divisors2 function from js_utils.js which includes all the
  divisors of a number, including 1 and n.

  (Sometimes factors mean prime factors, sometimes the specific prime
   factors that can recreate a number...)

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {all_divisors2,factorial,remove_duplicates2,range2} = require('./js_utils.js');


console.log("12345:", all_divisors2(12345));
console.log("120",all_divisors2(120));
console.log("720",all_divisors2(720));
console.log("7560",all_divisors2(7560));
console.log("9240",all_divisors2(9240));

console.log(range2(1,30)
            .map(n=>[n,all_divisors2(n).remove_duplicates2()]));

console.log(all_divisors2(2**11+1));

// Largest number of divisors: [ 7560, 64 ], [ 9240, 64 ]
console.log("\nSort on the number of divisors:");
console.log(range2(1,10_000)
            .map(n=>[n,all_divisors2(n).length])
            .sort((a,b)=>b[1]-a[1]));

console.log("\nDivisors of factorials");
console.log(range2(1,10)
            .map(n=>[`${n}!:`, factorial(n),all_divisors2(factorial(n))]));
