/* 

  Euler #39 in JavaScript.

  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
   
  {20,48,52}, {24,45,51}, {30,40,50}
   
  For which value of p <= 1000, is the number of solutions maximised?
  """


  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,sum2,max2,timing2} = require('./js_utils.js');

// 22ms
var euler39a = function() {
    var n = 1000;
    var squares = new Set(range2(1,n).map(i=>i*i));
    var valid = [];
    for (var x of squares) {
        for (var y of squares) {
            if (x < y && squares.has(x+y) && (Math.sqrt(x) + Math.sqrt(x) + Math.sqrt(x+y)) < 1000) {
                valid.push([x,y]);
            }
        }
    }
    var counts = {};
    for(var [x2,y2] of valid) {
       var c = Math.floor(Math.sqrt(x2) + Math.sqrt(y2) + Math.sqrt(x2+y2));
       if (!counts[c]) {
           counts[c] = 0;
       }
       counts[c] += 1;
    }

    // find max count
    var maxV = Object.values(counts).map(i=>parseInt(i)).max2();
    return Object.keys(counts)
    .filter(i=>counts[i] === maxV);

 
}

timing2(euler39a); // 22ms
