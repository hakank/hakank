/* 

  Euler #8 in JavaScript.

  Problem 8:
  """
  Find the greatest product of five consecutive digits in the 
  1000-digit number.
  ...
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {range,prod,timing2} = require('./js_utils.js');

// Keep it global so we don't get sore eyes...
const n = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450";

// 0ms
const euler8a = function() {
    const a = n.split("").map(i=>parseInt(i))
    const len = a.length;
    let max = 0;
    for(let i = 0; i < len-5+1; i++) {
        let p = 1;
        for (var j = 0; j < 5; j++) {
            p *= a[i+j];
            if (p===0) {
                continue;
            }
        }
        if (p > max) {
            max = p;
        }
    }
    return max;
}

// 1ms
const euler8b = function() {
    const a = n.split("").map(i=>parseInt(i))
    const len = a.length;
    let max = 0;
    for(let i = 0; i < len-5+1; i++) {
        var p = prod(a.slice(i,i+5));        
        if (p > max) {
            max=p;
        }
    }
    return max;
}

// 0ms
const euler8c = function() {
    const a = n.split("").map(i=>parseInt(i))
    return range(a.length-5+1)
        .map(i=> {
            return prod(a.slice(i,i+5));        
        })
        .max2();
}

// 0ms
// Not very functional programming...
const euler8d = function() {
    const a = n.split("").map(i=>parseInt(i));
    let max = 0;
    while (a.length > 0) {
        const p = prod(a.slice(0,5));
        a.shift()        
        if (p > max) {
            max = p;
        }
    }
    return max;
}


timing2(euler8a);
// timing2(euler8b);
// timing2(euler8c);
// timing2(euler8d);
