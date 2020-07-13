/*
  Euler #1 in JavaScript.

  Problem 1
  """
  If we list all the natural numbers below 10 that are multiples of 3 or 5, 
  we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';

// For sum()
// const math = require('mathjs'); 

const {sum,sum2,range,timing2} = require("./js_utils.js");

// I don't know how to export an Array extension...
// Array.prototype.sum = function() { return this.reduce((a,b)=>a+b); }

const euler1a = function() {
    var total = 0;
    var i = 0;
    while (++i <= 999) {
        if (i % 3 === 0 || i % 5 === 0) {
            total += i;
        }
    }
    return total;
}

const euler1b = function() {
  return Array.from({length:1000},(_, i)=>i).filter(i=>i%3===0||i%5===0).reduce((a,b)=>a+b); 
}

const mod3_or_5 = function(n) {
  return n % 3 === 0 || n % 5 == 0;
}

const euler1c = function() {
  return Array.from({length:1000},(_, i)=>i).filter(i=>mod3_or_5(i)).reduce((a,b)=>a+b);
}

const euler1d = function() {
    // using mathjs (see above)
    // return math.sum(Array.from({length:1000},(_, i)=>i).filter(i=>i%3===0||i%5===0));
    return Array.from({length:1000},(_, i)=>i).filter(i=>i%3===0||i%5===0).sum2();
}


const euler1e = function() {
    return sum(range(1000)
                     .filter(i=>i%3===0||i%5===0));
}

const euler1f = function() {
    return range(1000)
        .filter(i=>i%3===0||i%5===0)
        .reduce((a,b)=>a+b);
}

const euler1g = function() {
    return range(1000)
        .filter(i=>i%3===0||i%5===0)
        .sum2();
}

timing2(euler1a);
// timing2(euler1b);
// timing2(euler1c);
// timing2(euler1d);
// timing2(euler1e);
// timing2(euler1f);
// timing2(euler1g);

