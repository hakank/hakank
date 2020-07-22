/* 

  Transposing a non square matrix in JavaScript.


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {transpose,transpose2,zip,zip2} = require('./js_utils.js');

// Note: zip* are aliased to transpose*

// Transposing a square or non square matrix.
/*
function transpose(m) {
    const rows = m.length;
    const cols = m[0].length;
    const m2 = new Array(cols);    
    for(let j = 0; j < cols; j++) {
        m2[j] = new Array(rows);
        for(let i = 0; i < rows; i++) {
            m2[j][i] = m[i][j];
        }
    }
    return m2;
}
*/

console.log("A non square matrix");
let m = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]];
console.log(m);
let m2 = transpose(m);
console.log(m2);

m2 = zip(m);
console.log(m2);

console.log("A square matrix:");
m = [[1,2,3],[4,5,6],[7,8,9]];
console.log(m);
m2 = m.transpose2(m);
console.log(m2);

m2 = m.zip2(m);
console.log(m2);
