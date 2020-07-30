/* 

  McNuggets Problem in JavaScript.

  http://rosettacode.org/wiki/McNuggets_Problem
  """
  From Wikipedia [https://en.wikipedia.org/wiki/Coin_problem#McNugget_numbers];

    The McNuggets version of the coin problem was introduced by Henri Picciotto,
    who included it in his algebra textbook co-authored with Anita Wah. Picciotto
    thought of the application in the 1980s while dining with his son at
    McDonald's, working the problem out on a napkin. A McNugget number is
    the total number of McDonald's Chicken McNuggets in any number of boxes.
    In the United Kingdom, the original boxes (prior to the introduction of
    the Happy Meal-sized nugget boxes) were of 6, 9, and 20 nuggets.

  Task

  Calculate (from 0 up to a limit of 100) the largest non-McNuggets number (a number 
  n which cannot be expressed with 6x + 9y + 20z = n where x, y and z are 
  natural numbers). 
  """
  

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {max2,remove_duplicates2,flatten2,range,range2} = require('./js_utils.js');


// Plain loops
let a = new Array(100).fill(0);
for(let x = 0; x <= 16; x++) {
    for(let y = 0; y <= 1; y++) {
        for(let z = 0; z <= 5; z++) {
            let n = 6*x + 9*y + 20*z;
            if (n <= 100) {
                a[n]++;
            }
        }
    }
}
console.log(a
            .map((v,i)=>[i,v])
            .filter(v=>v[1]===0)
            .sort((a,b)=>b[0]-a[0])[0][0]
           );


// Functional approach
// Get all the possible combinations
const e =
      range(16)
      .map(x=>range(11)
           .map(y=>
                range(5)
                .map(z=> 6*x + 9*y + 20*z)))
      .flat().flat()
      .filter(n=>n<=100)
      .sort((a,b)=>a-b)
      .remove_duplicates2();
// Weed out the impossible combinations and get the largest
console.log(
    range(100)
        .filter(n=>
                e.every(i=>n!==i)).max2()
);
    

// It's simpler to use every direct
console.log(
    range(100)
        .map(n=>[n,
                 range(16)
                 .every(x=>range(11)
                        .every(y=>
                               range(5)
                               .every(z=> 6*x + 9*y + 20*z !== n)))
                ]
            )
        .filter(v=>v[1])
        .map(v=>v[0])
        .max2()
);


// And still a little shorter by removing filter from the above approach.
console.log(
    range(100)
        .map(n=>range(16)
                .every(x=>range(11)
                       .every(y=>
                              range(5)
                              .every(z=> 6*x + 9*y + 20*z !== n)))
            )
        .map((v,i)=>i*v)
        .max2()
);


