/* 

  Euler #6 in JavaScript.

  Problem 6
  """
  The sum of the squares of the first ten natural numbers is,
  1^(2) + 2^(2) + ... + 10^(2) = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)^(2) = 55^(2) = 3025

  Hence the difference between the sum of the squares of the first ten 
  natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one 
  hundred natural numbers and the square of the sum.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {sum,range2,timing2} = require('./js_utils.js');

// 1ms
var euler6a = function() {
    var s1 = sum(range2(1,100))**2;
    var s2 = sum(range2(1,100).map(i=>i**2))
    return s1-s2;
}

// 0ms
var euler6b = function() {
    return sum(range2(1,100))**2 -
        sum(range2(1,100).map(i=>i**2));
}


// timing2(euler6a);
timing2(euler6b);
