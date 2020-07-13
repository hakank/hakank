/* 

  Euler #20 in JavaScript.

  Problem 20:
  """
  n! means n (n 1) ... 3 2 1

  Find the sum of the digits in the number 100!")
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {prodN,sum2,range2,timing2} = require('./js_utils.js');


const factorialN = function(n) {
    return prodN(range2(1,n));
}

// 0ms
const euler20a = function() {
    return factorialN(100).toString().split("").map(i=>parseInt(i)).sum2();
}

timing2(euler20a);
