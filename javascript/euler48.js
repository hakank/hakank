/* 

  Euler #48 in JavaScript.

  Problem 48:
  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 9ms
var euler48a = function() {
    var sum = 0;
    var t = 10000000000;
    for(var i = 1; i <= 1000; i++) {
        var n = i;
        for(var j = 2; j <= i; j++) {
            n = (n * i) % t;
        }
        sum = (sum + n) % t;
    }
    
    return sum;
}

timing2(euler48a); // 9ms
