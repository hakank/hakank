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
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {sum,range2,timing2} = require('./js_utils.js');
const fs = require('fs');

const to_code = function(s) {
    return s.split("").map(c=>c.charCodeAt()-64);
}

// 15ms
const euler22a = function() {
    const words = fs.readFileSync("euler22_names.txt").toString().split(",");
    let a = 1;
    let s = 0;
    for(let word of words.sort()) {
        word = word.replace(/"/g,'');
        s += a*sum(to_code(word));
        a++;
    }
    return s;

}

// Functional (except for a, darn..)
// 15ms
const euler22b = function() {
    let a = 1;
    return fs.readFileSync("euler22_names.txt").toString().split(",").sort() 
        .map(word=>(a++)*sum(to_code(word.replace(/"/g,'')))).sum2();
}

timing2(euler22a);
// timing2(euler22b);


