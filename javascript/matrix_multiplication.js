/* 

  Matrix multiplication in JavaScript.

  http://rosettacode.org/wiki/Matrix_multiplication
  """
  Task

  Multiply two matrices together.

  They can be of any dimensions, so long as the number of columns of the first 
  matrix is equal to the number of rows of the second matrix.
  """

  See js_utils.js for implementations.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {matrix_mult,matrix_mult2,random_matrix,print_matrix2,print_matrix,timing2} = require('./js_utils.js');

const m1 = [[1,2,3],[4,5,6],[7,8,9]];
print_matrix(matrix_mult(m1,m1));

console.log()
let a = [[1,2],[3,4]];
let b = [[-3,-8,3],[-2,1,4]];
print_matrix(matrix_mult(a,b));

console.log()

a = random_matrix(18,20,100);
b = random_matrix(20,15,100);
console.log("a:");
print_matrix(a);
console.log("b:");
print_matrix(b);
console.log("result:");
a.matrix_mult2(b).print_matrix2();
// timing2(function test() { a.matrix_mult2(b)});


