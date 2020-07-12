/* 

  Euler #38 in JavaScript.

  """
  Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

  By concatenating each product we get the 1 to 9 pandigital, 
  192384576. We will call 192384576 the concatenated product of 192 
  and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
  concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be 
  formed as the concatenated product of an integer with 
  (1,2, ... , n) where n > 1?
  """

  This JavaScript model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript/

*/
'use strict';
const {is_pandigital,timing2} = require('./js_utils.js');


// 1ms
var euler38a = function() {
    for(var n = 9876; n >= 9; n--) {     
        var s = n.toString();
        var i = 2;
        while(s.length < 9) {
            s += (n*i).toString();
            i++;
        }
        if (s.length == 9 && is_pandigital(s)) {          
          return s;
        }
    }
    return undefined;
}

timing2(euler38a); // 1ms




