/* 

  Euler #31 in JavaScript.

  Problem 31
  """
  In England the currency is made up of pound, £, and pence, p, and 
  there are eight coins in general circulation:

     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

  It is possible to make £2 in the following way:

     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

  How many different ways can £2 be made using any number of coins?
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// DP approach
var coins = function(c,money,m) {
    var len = c.length;
    if (m === len) {
        return 1;
    }

    var sum1 = 0;
    for(var i = m; i <= len; i++) {
        if (money - c[i-1] === 0) {
            sum1++;
        }
        if (money - c[i-1] > 0) {
            sum1 += coins(c, money-c[i-1], i)
        }
    }
    return sum1;
}

// 6ms
var euler31a = function() {
    var c = [200,100,50,20,10,5,2,1];
    return coins(c, 200, 1);
}

timing2(euler31a); // 6ms

