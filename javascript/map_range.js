/* 

  Map range in JavaScript.

  http://rosettacode.org/wiki/Map_range
  """
  Given two ranges:

  - [a1, a2] and
  - [b1, b2] ;

  then a value s in range [a1, a2] 
  is linearly mapped to a value t in range [b1, b2] 

  where:

     t = b1 + (s−a1) (b2−b1)
              --------------
                 (a2−a1)

  Task

  Write a function/subroutine/... that takes two ranges and a real number, and 
  returns the mapping of the real number from the first to the second range.

  Use this function to map values from the range [0, 10] to the range [-1, 0].


  Extra credit
 
  Show additional idiomatic ways of performing the mapping, using tools available to the language. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2} = require('./js_utils.js');

//
// map s from domain a -> domain b
//
function map_range(s,a,b) {
    const [a1,a2] = a;
    const [b1,b2] = b;

    return b1 + ((s-a1)*(b2-b1))/(a2-a1);
}

let [a1,a2] = [0,10];
let [b1,b2] = [-1,0];

console.log([a1,a2],"to",[b1,b2]);
for(let i = a1; i <= a2; i+=1) {
    console.log(i, map_range(i,[a1,a2],[b1,b2])) ;
}

[a1,a2] = [-1,1];
[b1,b2] = [0,10];

console.log("\n",[a1,a2],"to",[b1,b2]);
for(let i = a1; i <= a2; i+=0.1) {
    console.log(i, map_range(i,[a1,a2],[b1,b2])) ;
}

[a1,a2] = [0,10];
[b1,b2] = [0,1000];

console.log("\n",[a1,a2],"to",[b1,b2]);
for(let i = a1; i <= a2; i+=1) {
    console.log(i, map_range(i,[a1,a2],[b1,b2])) ;
}

