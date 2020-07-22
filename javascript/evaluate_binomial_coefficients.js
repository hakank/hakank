/* 

  Evaluate binomial coefficients in JavaScript.

  http://rosettacode.org/wiki/Evaluate_binomial_coefficients
  """
  This programming task, is to calculate ANY binomial coefficient.

  However, it has to be able to output ( 5 3 ) (5 over 3), which is 10.

  This formula is recommended:

   ( n k ) = n!/((n−k)!*k! = (n*(n−1)*( n − 2 )...(n−k+1)) / (k(k−1)(k−2)...1)
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {binomial,binomialN,factorial,factorialN,range2,timing2} = require('./js_utils.js');

/*
// Moved to js_utils.js
function binomial(n,k) {
    if (k < 0 || k > n) {
        return 0;
    }
    return factorial(n)/(factorial(n-k)*factorial(k));
}

function binomialN(n,k) {
    if (k < 0 || k > n) {
        return 0;
    }
    return factorialN(n)/(factorialN(n-k)*factorialN(k));
}
*/

// Recursive version
function binomial_rec(n,k) {
    if (k < 0 || k > n) {
        return 0;
    } else if (k === 0 || k === n) {
        return 1;
    } else {
        return n * binomial_rec(n-1, k-1) / k;
    }
}

console.log(binomial(5,3));
console.log(binomial_rec(5,3));

console.log(range2(0,10)
    .map(n=>range2(0,n)
         .map(k=>binomial(n,k))));

console.log(binomialN(150,75));

// timing2(function testbig() {binomialN(100000,50000)}); // 16.346s

