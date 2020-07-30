/* 

  Leap year in JavaScript.

  http://rosettacode.org/wiki/Leap_year
  """
  Task

  Determine whether a given year is a leap year in the Gregorian calendar. 
  """
  
  https://en.wikipedia.org/wiki/Leap_year
  """
  ... year which is an integer multiple of 4 (except for years evenly divisible by 100, 
  which are not leap years unless evenly divisible by 400
  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2} = require('./js_utils.js');


function isLeapYear(year) {
    return  (year % 4 === 0 && year % 100 !== 0)
        ||
        (year % 400 === 0);

}

function nextLeapYear(year) {
    let i = 1;
    while (true) {
        if (isLeapYear(year+i)) {
            return year+i;
        }
        i++;
    }
    return undefined;
}


const years = [1899, 1900, 1901, 1902, 1903, 1904, 1905, 1999, 2000, 2001, 2002, 2003, 2004];

console.log(years.map(y=>[y,isLeapYear(y)]));


console.log(years.filter(y=>isLeapYear(y)));


console.log(range2(2000,2099).filter(y=>isLeapYear(y)));

console.log("next leap year: ", nextLeapYear(1900 + new Date().getYear()));
