/* 

  Euler #19 in JavaScript.

  Problem 19:
  """
  You are given the following information, but you may prefer 
  to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not 
    on a century unless it is divisible by 400.
  
  How many Sundays fell on the first of the month during the 
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {timing2} = require('./js_utils.js');

// 1ms
const euler19a = function() {
    let count = 0;
    for (let year = 1901; year <= 2000; year++) {
        for (let month = 1; month <= 12; month++) {
            if (new Date(year, month, 1).getDay() == 0) {
                count++;
            }
        }
    }
    
    return count;
}

timing2(euler19a);
