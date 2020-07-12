/* 

  Euler #5 in JavaScript.

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers 
  from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers 
  from 1 to 20?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {range2,timing2,lcm} = require('./js_utils.js');

// 0ms
var euler5a = function() {
    var a = 1;
    for(var i = 2; i <= 20; i++) {
        a = lcm(a,i)
    }
    return a;
}

// Nicer.
// 0ms
var euler5b = function() {
    return range2(2,20).reduce((i,j)=>lcm(i,j));
}

// timing2(euler5a); // 0ms
timing2(euler5b); // 0ms
