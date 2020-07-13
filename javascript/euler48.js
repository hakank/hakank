/* 

  Euler #48 in JavaScript.

  Problem 48:
  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 9ms
const euler48a = function() {
    let sum = 0;
    const t = 10_000_000_000;
    for(let i = 1; i <= 1_000; i++) {
        let n = i;
        for(let j = 2; j <= i; j++) {
            n = (n * i) % t;
        }
        sum = (sum + n) % t;
    }
    
    return sum;
}

timing2(euler48a); // 9ms
