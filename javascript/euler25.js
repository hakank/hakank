/* 

  Euler #25 in JavaScript.

  Problem 25:
  """
  The Fibonacci sequence is defined by the recurrence relation:

     Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
  
  Hence the first 12 terms will be:

     F1 = 1
     F2 = 1
     F3 = 2
     F4 = 3
     F5 = 5
     F6 = 8
     F7 = 13
     F8 = 21
     F9 = 34
     F10 = 55
     F11 = 89
     F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?")
  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {fibN,memoizer,timing2} = require('./js_utils.js');

/*
// Is not faster
var fib_len_memo = memoizer(function(n) {
    return fibN(BigInt(n)).toString().length;
})
*/

var fib_len = function(n) {
    return fibN(BigInt(n)).toString().length;
}

// Brute force: 134ms
var euler25a = function() {
    var i = 1;
    var len = 0;
    while (len < 1000) {
        len = fibN(BigInt(i)).toString().length;
        i++;
    }

    return i;
}

//
// Using some heuristics to find the upper limit
// (from my Picat solution).
// 10ms
var euler25b = function() {
    var target = 1000;
    var foundUpper = 0;
    var i = 1;
    var fibLen = 0;
    var step = 43;
    // Get the upper limit
    while (fibLen < target && foundUpper === 0) {
        fibLen = fib_len(step*i);
        if (fibLen > target) {
            foundUpper = i;
            break;
        }
        i++;
    }

    // Now check all numbers from Step*(FoundUpper-1) .. Step*FoundUpper
    // The target must be in that interval.
    var f = step*(foundUpper-1);
    fibLen = fib_len(f);
    while(fibLen < target && f <= step*foundUpper) {
        fibLen = fib_len(f);
        f++;
    }

    return f;
}

// timing2(euler25a); // 134ms
timing2(euler25b); // 10ms
