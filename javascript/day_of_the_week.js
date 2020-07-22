/* 

  Day of the week in JavaScript.

  http://rosettacode.org/wiki/Day_of_the_week
  """
  A company decides that whenever Xmas falls on a Sunday they will give their workers 
  all extra paid holidays so that, together with any public holidays, workers will 
  not have to work the following week (between the 25th of December and the first of January).

  Task

  In what years between 2008 and 2121 will the 25th of December be a Sunday?

  Using any standard date handling libraries of your programming language; compare the 
  dates calculated with the output of other languages to discover any anomalies in the 
  handling of dates which may be due to, for example, overflow in types used to represent 
  dates/times similar to y2k type problems.
  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {range2} = require('./js_utils.js');

const dec = 11;
console.log(range2(2008,2121)
            .filter(year=>new Date(year, dec, 25).getDay() === 0));
