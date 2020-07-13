/* 

  Euler #36 in JavaScript.

  Problem 36:
  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {range2,sum2,palindromic_list,dec2base,timing2} = require('./js_utils.js');

// 129ms
const euler36a = function() {
    let s = 0;
    for(let n = 1; n <= 999999; n++) {
        if (palindromic_list(n.toString().split("")) &&
            palindromic_list(dec2base(n,2))) {
            s += n;
        }
    }
    return s;
}

// Slower: 280ms
const euler36b = function() {
    return range2(1,999999)
        .filter(n=>palindromic_list(n.toString().split("")))
        .filter(n=>palindromic_list(dec2base(n,2)))
        .sum2();
}


timing2(euler36a); // 129ms
// timing2(euler36b); // 280ms
