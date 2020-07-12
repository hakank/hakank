/* 

  Euler #15 in JavaScript.

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.
  
  How many routes are there through a 20×20 grid?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,prod,timing2} = require('./js_utils.js');

var prodlist = function(from,to) {
    return prod(range2(from,to));
}

// 0ms
var euler15a = function() {
    return prodlist(21,40) / prodlist(2,20);
}


timing2(euler15a);
