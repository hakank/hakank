/* 

  Euler #30 in JavaScript.

  Problem 30  
  """
  Surprisingly there are only three numbers that can be written 
  as the sum of fourth powers of their digits:

     1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
     8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
     9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)

  As 1 = 1^(4) is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of 
  fifth powers of their digits.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,sum2,memoizer, timing2} = require('./js_utils.js');

// 80ms
var euler30a = function() {
    var t = 0;
    var m = 5;
    for(var n = 10; n <= 6*9**5; n++) {
        var nn = n.toString().split("").map(i=>parseInt(i)**m).sum2();
        if (n === nn) {
            t += n;
        }
    }
    return t;
}

// 282ms
var euler30b = function() {
    var m = 5;
    return range2(10,6*9**5)
        .map(n=>{return [n,n.toString().split("").map(i=>parseInt(i)**m).sum2()]})
        .filter(nn=>nn[0] === nn[1])
        .map(nn=>nn[0])
        .sum2();
}

timing2(euler30a); // 80ms
// timing2(euler30b); // 282,s
