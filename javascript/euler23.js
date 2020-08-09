/* 

  Euler #23 in JavaScript.

  Problem 23:
  """
  A perfect number is a number for which the sum of its proper divisors 
  is exactly equal to the number. For example, the sum of the proper divisors 
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than 
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
  that can be written as the sum of two abundant numbers is 24. By mathematical 
  analysis, it can be shown that all integers greater than 28123 can be written 
  as the sum of two abundant numbers. However, this upper limit cannot be reduced 
  any further by analysis even though it is known that the greatest number that 
  cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of 
  two abundant numbers.
  """ 


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 22ms
// Note: This is a solution ported from Picat code which in turn is from C++(?) code,
const euler23a = function() {
    const limit = 20161;
    let arr = new Array(limit+1).fill(1);
    
    for (let i = 2; i < limit + 1; i++) {
        for (let j = i * 2; j <= limit; j = j + i) {
            arr[j] = arr[j] + i;
        }
    }
    let abundant = [];
    for (let i = 12; i <= limit; i++) {
        if (arr[i] > i) {
            abundant.push(i);
        }
    }
    for (let a of abundant) {
        for (let b of abundant) {
            if (b > a || a + b >= limit) {
                break;
            } else {
                arr[a + b] = 0;
            }
        }
    }
    let s = 0;
    for (let i = 1; i <= limit; i++) {
        if (arr[i] != 0) {
            s += i;
        }
    }
    return s;

}

// Slightly different loop when checking abundant
// And slightly slower: 28ms
const euler23b = function() {
    const limit = 20161;
    let arr = new Array(limit+1).fill(1);
    
    for (let i = 2; i < limit + 1; i++) {
        for (let j = i * 2; j <= limit; j = j + i) {
            arr[j] = arr[j] + i;
        }
    }
    let abundant = [];
    for (let i = 12; i <= limit; i++) {
        if (arr[i] > i) {
            abundant.push(i);
        }
    }
    // Testing a different loop here
    const len = abundant.length;
    for (let i = 0; i<len; i++) {
        for (let j = 0; j<=i; j++) {
            let a = abundant[i];
            let b = abundant[j];
            if (a + b <= limit) {
                arr[a + b] = 0;
            }
        }
    }

    let s = 0;
    for (let i = 1; i <= limit; i++) {
        if (arr[i] != 0) {
            s += i;
        }
    }
    return s;

}


timing2(euler23a);

// timing2(euler23b);

