/* 

  Euler #39 in JavaScript.

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """

  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {range2,sum2,max2,timing2} = require('./js_utils.js');

// 22ms
const euler39a = function() {
    const n = 1000;
    const squares = new Set(range2(1,n).map(i=>i*i));
    let valid = [];
    for (let x of squares) {
        for (let y of squares) {
            if (x < y && squares.has(x+y) && (Math.sqrt(x) + Math.sqrt(x) + Math.sqrt(x+y)) < 1000) {
                valid.push([x,y]);
            }
        }
    }
    let counts = {};
    for(let [x2,y2] of valid) {
       const c = Math.floor(Math.sqrt(x2) + Math.sqrt(y2) + Math.sqrt(x2+y2));
       if (!counts[c]) {
           counts[c] = 0;
       }
       counts[c] += 1;
    }

    // find max count
    const maxV = Object.values(counts).map(i=>parseInt(i)).max2();
    return parseInt(Object.keys(counts)
                    .filter(i=>counts[i] === maxV)[0]);

 
}

timing2(euler39a); // 22ms
