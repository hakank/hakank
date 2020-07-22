/* 

  Gray code in JavaScript.

  http://rosettacode.org/wiki/Gray_code
  """
  Task
  Gray code

  You are encouraged to solve this task according to the task description, 
  using any language you may know.

  Gray code is a form of binary encoding where transitions between consecutive 
  numbers differ by only one bit. This is a useful encoding for reducing 
  hardware data hazards with values that change rapidly and/or connect to 
  slower hardware as inputs. It is also useful for generating inputs for 
  Karnaugh maps in order from left to right or top to bottom.

  Create functions to encode a number to and decode a number from Gray code.

  Display the normal binary representations, Gray code representations, and 
  decoded Gray code values for all 5-bit binary numbers (0-31 inclusive, 
  leading 0's not necessary).

  There are many possible Gray codes. The following encodes what is called 
  "binary reflected Gray code."

  Encoding (MSB is bit 0, b is binary, g is Gray code):

  if b[i-1] = 1
     g[i] = not b[i]
  else
     g[i] = b[i]

  Or:

  g = b xor (b logically right shifted 1 time)

  Decoding (MSB is bit 0, b is binary, g is Gray code):

  b[0] = g[0]

  for other bits:
  b[i] = g[i] xor b[i-1]

  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {gray_encode,gray_decode,nlen} = require('./js_utils.js');

/*
// Moved to js_utils.js
function gray_encode(n) {
    return n ^ (n >> 1);
}
function gray_decode(n) {
    let p = n;
    n = n >> 1;
    while (n != 0) {
        p = p ^ n;
        n = n >> 1;
    }
    return p;
}
*/

function show_gray(r) {
    console.log(`\nShow gray codes from 0 to ${2**r-1}`);
    const len = nlen(2**r);
    for(let i = 0; i < 2**r; i++) {
        const genc = gray_encode(i);
        const gdec = gray_decode(i);
        console.log(i.toString(10).padStart(len),
                    genc.toString(10).padStart(r,0),
                    genc.toString(2).padStart(r,0),
                    gdec.toString(10).padStart(len),
                   );
    }
    
}

show_gray(4);
show_gray(8);
