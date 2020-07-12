/* 

  Euler #21 in JavaScript.

  Problem 21
  """
  Let d(n) be defined as the sum of proper divisors of n (numbers less 
  than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
  pair and each of a and b are called amicable numbers.
  
  For example, the proper divisors of 220 are 
  1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
  The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
  
  Evaluate the sum of all the amicable numbers under 10000.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,all_divisors3,sum,sum2,timing2} = require('./js_utils.js');


// 153ms
var euler21a = function() {
    var s = {};
    for(var a = 1; a <= 9999; a++) {
        var b = sum(all_divisors3(a));
        var c = sum(all_divisors3(b));
        if (a !== b && a === c) {
            s[a] = 1;
            s[b] = 1;
        }
    }
    return Object.keys(s).map(i=>parseInt(i)).sum2();
}


// Little more functional
// 160ms
var euler21b = function() {
    var s = {};
    var a = range2(1,9999)
        .map(a=> {
            var b = sum(all_divisors3(a));
            var c = sum(all_divisors3(b));
            return [a,b,c];
        })
        .filter(v=>v[0]!==v[1] && v[0] === v[2])
        .map(v=>v[0]).sum2()
                
    return a;
}

timing2(euler21a);
// timing2(euler21b);


