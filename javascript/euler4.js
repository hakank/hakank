/* 

  Euler #4 in JavaScript.

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.
  """


  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {range2,max2,palindromic_number, timing2} = require('./js_utils.js');


// 8ms
var euler4a = function() {
    var max = 0;
    var from = 100;
    var to = 999;
    for(var i = from; i <= to; i++) {
        for(var j = i; j <= to; j++) {
            var ij = i*j;
            if (ij > max && palindromic_number(ij)) {
                max = ij;
            }
        }
    }
    return max;
}

// Much slower: 47ms
var euler4b = function() {
    var from = 100;
    var to = 999;
    var a = [];
    range2(100,999)
        .forEach(i=> {
            range2(i,to)
                .forEach(j=> {
                    var ij = i*j;
                    if (palindromic_number(ij)) {
                        a.push(ij)
                    }
                })
                 
        })
    return a.max2();
}

// 46ms
var euler4c = function() {
    var from = 100;
    var to = 999;
    // This yield an array of arrays,
    // so we have to flatten it.
    return range2(100,999)
        .map(i=> {
            return range2(i,to)
                .map(j=>i*j)
                .filter(ij=>palindromic_number(ij))
        }).flatten2().max2();
}


timing2(euler4a);
// timing2(euler4b);
// timing2(euler4c);

