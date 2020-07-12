/* 

  Euler #22 in JavaScript.

  Problem 22:
  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K 
  text file containing over five-thousand first names, begin by sorting 
  it into alphabetical order. Then working out the alphabetical value 
  for each name, multiply this value by its alphabetical position in the 
  list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, 
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
  the list. So, COLIN would obtain a score of 938 53 = 49714.

  What is the total of all the name scores in the file?")
  """


  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {sum,range2,timing2} = require('./js_utils.js');
const fs = require('fs');

var to_code = function(s) {
    return s.split("").map(c=>c.charCodeAt()-64);
}

// 15ms
var euler22a = function() {
    var words = fs.readFileSync("euler22_names.txt").toString().split(",");
    var a = 1;
    var s = 0;
    for(var word of words.sort()) {
        word = word.replace(/"/g,'');
        s += a*sum(to_code(word));
        a++;
    }
    return s;

}

// Functional (except for a, darn..)
// 15ms
var euler22b = function() {
    var a = 1;
    var s = fs.readFileSync("euler22_names.txt").toString().split(",").sort() 
        .map(word=>(a++)*sum(to_code(word.replace(/"/g,'')))).sum2();
    
    return s;
}

// timing2(euler22a);
timing2(euler22b);


