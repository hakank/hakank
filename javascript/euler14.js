/* 

  Euler #14 in JavaScript.

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following 
  sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1) 
  contains 
  10 terms. Although it has not been proved yet (Collatz Problem), it is 
  thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
var {memoizer,timing2} = require('./js_utils.js');


// It's must slower with memoizing
/*
var hailstone = memoizer(function(n) {
    if (n % 2 === 0) {
        return n / 2;
    } else {
        return 3*n+1;
    }
})
*/

var hailstone = function(n) {
    if (n % 2 === 0) {
        return n / 2;
    } else {
        return 3*n+1;
    }
}

// 
// 4443ms (4.4s)
// Using 3..2..10000: 2859ms (2.9s)
var euler14a = function() {
    var maxN = 0;
    var maxLen = 0;
    // for(var n = 1; n < 1000000; n++) {
    for(var n = 3; n < 1000000; n+=2) {        
        var m = n;
        var alen = 0;
        while (m > 1) {
            m = hailstone(m);
            alen++;
        }
        alen++;
        if (alen > maxLen) {
            maxN = n;
            maxLen = alen;
        }
    }
    return maxN;
}

//
// Cache the lengths as well.
//
// 1..999999: 156ms
// 3..2..999999: 163ms (slower!???)
var euler14b = function() {
    var hash = new Object;
    var maxN = 0;
    var maxLen = 0;
    for(var n = 2; n < 1000000; n++) {        
    // for(var n = 3; n < 1000000; n+=2) {        
        var m = n;
        var mlen = 1;
        while (m > 1) {
            if (hash[m]) {
                mlen = hash[m]+mlen-1;
                m = 1;
            } else {
                m = hailstone(m);
                mlen++;
            }
        }
        if (!hash[n]) {
            hash[n] = mlen;
        }
        if (mlen > maxLen) {
            maxN = n;
            maxLen = mlen;
        }
    }
    return maxN;
}

// timing2(euler14a); // 630ms
timing2(euler14b); // 153ms
