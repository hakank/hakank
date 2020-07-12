/* 

  Euler #17 in JavaScript.

  Problem 17:
  """
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  
  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
  words, how many letters would be used?
  
  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 
  "and" when writing out numbers is in compliance with British usage.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/

'use strict';
const {range2,sum2,timing2} = require('./js_utils.js');


var english = function(n) {
    var divs  =      [1000000000, 1000000,  1000,       100];
    var divnames  =  ["billion", "million", "thousand", "hundred"];
    var prefixes  =  ["","0", "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"];
    var _ordinals  = ["","first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                      "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
                      "fourteenth","fifteenth", "sixteenth", "seventeenth",
                      "eighteenth", "nineteenth"];
    var cardinals =  ["","one", "two", "three", "four", "five", "six", "seven",
                      "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
    
    var s = "";
    var printed = 0;
    if (n < 0) {
        s = "minus" + s;
        n = -n;
    }
    var d = 0;
    for(var i = 0; i < divs.length; i++) {
        d = Math.floor(n / divs[i]);
        n %= divs[i];
        if (d != 0) {
            s += english(d) + divnames[i];
            printed = 1;
        }
    }
    
    if (n > 0 && printed === 1) {
        s += "and";
    }
    if (n === 0) {
        // dummy
    } else if (n > 19) {
        d = Math.floor(n / 10);
        n %= 10;
        s += prefixes[d] + "ty" + english(n);
    } else {
        s += cardinals[n];
    }
    
    return s;
}

// 3ms
var euler17a = function() {
    var total = 0;
    return range2(1,1000)
        .map(i=>english(i).length)
        .sum2()

}

timing2(euler17a);

