/* 

  Monte Carlo methods in JavaScript.

  http://rosettacode.org/wiki/Monte_Carlo_methods
  """
  A Monte Carlo Simulation is a way of approximating the value of a function 
  where calculating the actual value is difficult or impossible.
  It uses random sampling to define constraints on the value and 
  then makes a sort of "best guess."

  A simple Monte Carlo Simulation can be used to calculate the value for π.

  If you had a circle and a square where the length of a side of the square 
  was the same as the diameter of the circle, the ratio of the area of the 
  circle to the area of the square would be π/4.

  So, if you put this circle inside the square and select many random points 
  inside the square, the number of points inside the circle divided by the 
  number of points inside the square and the circle would be approximately 
  π/4.


  Task

  Write a function to run a simulation like this, with a variable number of 
  random points to select.

  Also, show the results of a few different sample sizes.

  For software where the number π is not built-in, we give π as a 
  number of digits:

            3.141592653589793238462643383280
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range,range2} = require('./js_utils.js');

// Function to use for the simulation
function pi_f() {
    return Math.random()**2 + Math.random()**2 <= 1.0 ? 1.0 : 0.0;
}


// Loop version: faster
function sim(n,f) {
    let c = 0;
    for(let i = 0; i < n; i++) {
        c += f();
    }
    return c;
}

// Functional (slower since we have to create large lists)
function sim2(n,f) {
    return range(n).map(m=>f()).sum2();
}

// Simulation of Pi
function sim_pi(n) {
    return 4.0*sim(n,pi_f)/n;
}

// Sumulation of pi
function sim_pi2(n) {
    return 4.0*sim2(n,pi_f)/n;
}


const tests = range2(1,9).map(n=>10**n);
console.log(tests);

//
// [10,...,10**7]: 0.3s
//
// For [10,.....10**9] (23.6s)
// sims: [
//           2.8,       3.52,
//         3.128,       3.14,
//       3.14848,   3.142624,
//     3.1413288, 3.14158048,
//   3.141604176
// ]
// Errors:
// [
//   0.3415926535897933,
//   -0.3784073464102069,
//   0.013592653589793002,
//   0.0015926535897929917,
//   -0.006887346410207051,
//   -0.001031346410206968,
//   0.0002638535897929728,
//   0.000012173589793107453,
//   -0.000011522410206854516
// ]
//
console.log("Testing sim/sim_pi");
const sims = tests.map(n=>sim_pi(n));

console.log("sims:", sims);
console.log("Errors:");
console.log(
    sims.map(p=>Math.PI-p)
);

// upto 10**7: 1.2s
// console.log("\nTesting sim2/sim_pi2");
// const sims2 = tests.map(n=>sim_pi2(n));

// console.log("sims2:", sims2);
// console.log("Errors:");
// console.log(
//     sims2.map(p=>Math.PI-p)
// );
