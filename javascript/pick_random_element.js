/* 

  Pick random element in JavaScript.

  http://rosettacode.org/wiki/Pick_random_element
  """
  Demonstrate how to pick a random element from a list. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {random_element,range,randomInt} = require('./js_utils.js');

/*
// Moved to js_utils.js
function random_element(a) {
    return a[randomInt(a.length)];
}
*/

let a = [1,2,3,4,19,21,3];

console.log(random_element(a));

range(10)
    .forEach(i=>console.log(random_element(a)));

range(10)
    .forEach(i=>console.log(random_element("håkan kjellerstrand")));
