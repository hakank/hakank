/* 

  Euler #42 in JavaScript.

  Problem 42:
  """
  The nth term of the sequence of triangle numbers is given by, 
      tn = 1/2*n*(n+1); 
  so the first ten triangle numbers are:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  By converting each letter in a word to a number corresponding to its 
  alphabetical position and adding these values we form a word value. For example, 
  the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value 
  is a triangle number then we shall call the word a triangle word.

  Using words.txt (right click and 'Save Link/Target As...'), a 16K text file 
  containing nearly two-thousand common English words, how many 
  are triangle words?
  """

  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range2,sum2,timing2} = require('./js_utils.js');
const fs = require("fs");


// n'th triangle number
const triangle_number = function(n) {
    return Math.floor((n*(n-1)) / 2);
}

// get the score for a name
const get_score = function(name) {
    return name.split("")
    .map(c=>c.charCodeAt()-64)
    .sum2();
}

// 3ms
const euler42a = function() {    
    const t20 = new Set(range2(1,20).map(i=>triangle_number(i)));
    const words = fs.readFileSync("euler42_words.txt","utf8").replace(/"/g,"").split(/,/);
    return words
    .filter(word => t20.has(get_score(word)))
    .length;    
}

timing2(euler42a); // 3ms

