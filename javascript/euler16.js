/* 

  Euler #16 in JavaScript.

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
  What is the sum of the digits of the number 2^1000?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {sum2,timing2} = require('./js_utils.js');

// 0ms
const euler16a = function() {
    return (2n**1000n).toString().split("").map(i=>parseInt(i)).sum2();
}

timing2(euler16a);
