/* 

  (Matrix) Element-wise operations in JavaScript.

  http://rosettacode.org/wiki/Element-wise_operations
  """
  Task

  Implement basic element-wise matrix-matrix and scalar-matrix operations, 
  which can be referred to in other, higher-order tasks.

  Implement:

  - addition
  - subtraction
  - multiplication
  - division
  - exponentiation


  Extend the task if necessary to include additional basic operations, which 
  should not require their own specialised task.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {create_matrix,matrix_element_op,matrix_element_op2,matrix_op,matrix_op2,random_matrix,print_matrix2,print_matrix,timing2} = require('./js_utils.js');



let m = [[1,2,3],[4,5,6],[7,8,9]];

print_matrix(m);

console.log("\nn+8");
print_matrix(m.matrix_element_op2((n)=>n+8));

console.log("\nn-1/2");
print_matrix(m.matrix_element_op2((n)=>n-1/2));

console.log("\nn*2");
print_matrix(m.matrix_element_op2((n)=>n*2));

console.log("\nn odd ? n*3+1 : n/2");
print_matrix(m.matrix_element_op2((n)=>n % 2 === 1 ? 3*n+1 : n/2));


console.log("\n2**n");
print_matrix(m.matrix_element_op2((n)=>2**n));

console.log("\nn**n");
print_matrix(m.matrix_element_op2((n)=>n**n));

console.log("\nMath.exp(n)");
print_matrix(m.matrix_element_op2((n)=>Math.exp(n)));

// array version
let a = [1,2,3,4,5,6];
console.log("\na:",a);
console.log(a.matrix_element_op2((n)=>n*2));



let m1 = random_matrix(3,3,10);
let m2 = random_matrix(3,3,10);
console.log("m1:");
print_matrix(m1);
console.log("m1:");
print_matrix(m2);
console.log("m1 + m2:");
print_matrix(m1.matrix_matrix_element_op2(m2,(a,b)=>a+b));

console.log("\nm1**m2:");
print_matrix(m1.matrix_matrix_element_op2(m2,(a,b)=>a**b+b**a));

let m3 = random_matrix(3,3,10);
print_matrix(m3);
console.log("\nm1*m2*m3 (matrix multiplication)");
print_matrix(m1.matrix_mult2(m2).matrix_mult2(m3));
