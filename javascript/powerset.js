/* 

  Power set in JavaScript.

  http://rosettacode.org/wiki/Power_set
  """
  A set is a collection (container) of certain values, without 
  any particular order, and no repeated values. It corresponds 
  with a finite set in mathematics. A set can be implemented 
  as an associative array (partial mapping) in which the value 
  of each key-value pair is ignored.

  Given a set S, the power set (or powerset) of S, written P(S), 
  or 2S, is the set of all subsets of S.

  Task : By using a library or built-in set type, or by defining 
  a set type with necessary operations, write a function with a set 
  S as input that yields the power set 2S of S.

  For example, the power set of {1,2,3,4} is 
   {{}, {1}, {2}, {1,2}, {3}, {1,3}, {2,3}, {1,2,3}, {4}, {1,4}, 
   {2,4}, {1,2,4}, {3,4}, {1,3,4}, {2,3,4}, {1,2,3,4}}. 

  Task

  By using a library or built-in set type, or by defining 
  a set type with necessary operations, write a function with a set 
  S as input that yields the power set 2**S of S.

  For example, the power set of {1,2,3,4} is 
   {{}, {1}, {2}, {1,2}, {3}, {1,3}, {2,3}, {1,2,3}, {4}, {1,4}, 
   {2,4}, {1,2,4}, {3,4}, {1,3,4}, {2,3,4}, {1,2,3,4}}. 

  Task

  By using a library or built-in set type, or by defining a set type with necessary 
  operations, write a function with a set S as input that yields the power set 2S of S.

  For example, the power set of {1,2,3,4}is

    {{}, {1}, {2}, {1,2}, {3}, {1,3}, {2,3}, {1,2,3}, {4}, {1,4}, {2,4}, 
    {1,2,4}, {3,4}, {1,3,4}, {2,3,4}, {1,2,3,4}}.

  For a set which contains n elements, the corresponding power set has 2**n elements, 
  including the edge cases of empty set.

  The power set of the empty set is the set which contains itself (2**0 = 1):

         P = {∅} 

  And the power set of the set which contains only the empty set, has two subsets, 
  the empty set and the set which contains the empty set (2**1 = 2):

  P = {∅,{∅}}

  Extra credit: Demonstrate that your language supports these last two powersets.
  """


  See js_utils.js for the implementation.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {powerset_array,range,timing} = require('./js_utils.js');

let r = range(4);
let p = powerset_array(r);
console.log(p);
console.log("length:", p.length);

p = powerset_array([]);
console.log(p);
console.log("length:", p.length);

console.log("\ncardinality of powersets for different n");
for(let n = 0; n <= 20; n++) {
    const theoretical = 2**n;
    console.log(`${n}: ${powerset_array(range(n)).length}  (${theoretical})`);
}





