/* 

  Combinations with repetitions in JavaScript.

  http://rosettacode.org/wiki/Combinations_with_repetitions
  """
  Combinations with repetitions

  Task
  You are encouraged to solve this task according to the task description, using any 
  language you may know.

  The set of combinations with repetitions is computed from a set, S (of cardinality n), 
  and a size of resulting selection, k, by reporting the sets of cardinality k where 
  each member of those sets is chosen from S. In the real world, it is about choosing 
  sets where there is a "large" supply of each type of element and where the order 
  of choice does not matter. For example:

  Q: How many ways can a person choose two doughnuts from a store selling three types 
     of doughnut: iced, jam, and plain? (i.e., S is { i c e d , j a m , p l a i n }, 
     | S | = 3, and k = 2.

  A: 6: {iced, iced}; {iced, jam}; {iced, plain}; {jam, jam}; {jam, plain}; {plain, plain}.

  Note that both the order of items within a pair, and the order of the pairs given in the answer 
  is not significant; the pairs represent multisets.

  Also note that doughnut can also be spelled donut.


  Task

  Write a function/program/routine/.. to generate all the combinations with repetitions of n 
  types of things taken k at a time and use it to show an answer to the doughnut example above.

  For extra credit, use the function to compute and show just the number of ways of choosing 
  three doughnuts from a choice of ten types of doughnut. 
  Do not show the individual choices for this part.
  """

  See js_utils.js for the implementation of combinations_with_replacements/2.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {combinations_with_replacements,range,range2} = require('./js_utils.js');

console.log(combinations_with_replacements(2,["iced","jam","plain"]));
console.log(combinations_with_replacements(3,range(10)).length);

console.log(range2(1,10).map(n=>combinations_with_replacements(3,n).length));
console.log(range2(1,10).map(n=>combinations_with_replacements(n,10).length));
