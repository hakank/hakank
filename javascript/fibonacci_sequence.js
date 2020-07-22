/* 

  Fibonacci sequence in JavaScript.

  http://rosettacode.org/wiki/Fibonacci_sequence
  """
  You are encouraged to solve this task according to the task description, using 
  any language you may know.

  The Fibonacci sequence is a sequence Fn of natural numbers defined recursively:

      F0 = 0 
      F1 = 1 
      Fn = Fn-1 + Fn-2, if n>1 


  Task

  Write a function to generate the nth Fibonacci number.

  Solutions can be iterative or recursive (though recursive solutions are 
  generally considered too slow and are mostly used as an exercise 
  in recursion).

  The sequence is sometimes extended into negative numbers by using a 
  straightforward inverse of the positive definition:

      Fn = Fn+2 - Fn+1, if n<0   

  support for negative n in the solution is optional. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {fib,fibN,range,range2,memoizer} = require('./js_utils.js');

//
// This version also supports negative numbers
// For negative numbers, the results alternative between
// positive and negative numbers.
// See https://en.wikipedia.org/wiki/Fibonacci_number#Negafibonacci
// (-8..8)
// −21, 13. −8. 5, −3, 2, −1, 1, 0, 1, 1, 2, 3, 5, 8,13,21
//
const fibNeg = memoizer(function(n) {
    if (n < 0) {
        return fibNeg(n+2) - fibNeg(n+1);
    } else if (n===0) {
        return 0;
    } else if (n === 1 || n == 0) {
        return 1;
    } else {
        return fibNeg(n-1)+fibNeg(n-2);
    }
})

console.log(range2(1,10).map(n=>fib(n)));

console.log("BigInt version");
console.log(range2(1,100).map(n=>fibN(BigInt(n))));
console.log("1000!:", fibN(1000n));

//
// Negative numbers:
// [ -8, -21 ]
// [ -7, 13 ]
// [ -6, -8 ]
// [ -5, 5 ]
// [ -4, -3 ]
// [ -3, 2 ]
// [ -2, -1 ]
// [ -1, 1 ]
// [ 0, 0 ]
// [ 1, 1 ]
// [ 2, 1 ]
// [ 3, 2 ]
// [ 4, 3 ]
// [ 5, 5 ]
// [ 6, 8 ]
// [ 7, 13 ]
// [ 8, 21 ]
//
console.log("Negative numbers");
range2(-8,8)
    .map(n=>[n,fibNeg(n)])
    .forEach(ni=>console.log(ni));
