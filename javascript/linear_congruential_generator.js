/* 

  Linear congruential generator in JavaScript.

  http://rosettacode.org/wiki/Linear_congruential_generator
  """
  The linear congruential generator is a very simple example of a random number 
  generator. All linear congruential generators use this formula:

    r_{n + 1} = a \times r_n + c \pmod m 

  Where:

    r0 is a seed.
    r1, r2, r3, ..., are the random numbers.
    a, c, m are constants. 

  If one chooses the values of a, c and m with care, then the generator produces a 
  uniform distribution of integers from 0 to m − 1.

  LCG numbers have poor quality. rn and rn + 1 are not independent, as true random 
  numbers would be. Anyone who knows rn can predict rn + 1, therefore LCG is not 
  cryptographically secure. The LCG is still good enough for simple tasks like 
  Miller-Rabin primality test, or FreeCell deals. Among the benefits of the LCG, 
  one can easily reproduce a sequence of numbers, from the same r0. One can also 
  reproduce such sequence with a different programming language, because the formula 
  is so simple.

  The task is to replicate two historic random number generators. One is the rand() 
  function from BSD libc, and the other is the rand() function from the Microsoft 
  C Runtime (MSCVRT.DLL). Each replica must yield the same sequence of integers 
  as the original generator, when starting from the same seed.

  In these formulas, the seed becomes state0. The random sequence is rand1, rand2 and so on.

  BSD formula:

    state{n + 1} = 1103515245 * state{n} + 12345 mod 2^31
    rand{n} = state{n}
    rand{n} is in range 0 to 2147483647. 

  Microsoft formula:

    state{n + 1} = 214013 * state{n} + 2531011 mod 2^31
    rand{n} = state{n} div 2^16
    rand{n} is in range 0 to 32767. 

  The BSD formula was so awful that FreeBSD switched to a different formula. More 
  info is at Random number generator (included)#C. 
  """

  Note: It seems that for bsd and lcg we have to use BigInt since
    seed*multiplier
  might > Number.MAX_SAFE_INTEGER. For ms it's ok with plain integers.

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {range,range2} = require('./js_utils.js');

let BSD_STATE = 0n; // BigInt
let MS_STATE  = 0;  // Plain integer

let LCG_HASH = {};

// BSD
function bsd_seed(seed) {
    BSD_STATE = seed || 0n; // BigInt
    return BSD_STATE;
}


function bsd() {
    let seed = BSD_STATE || 0n; // BigInt
    let rand = (1103515245n*seed + 12345n) % 2n**31n;
    BSD_STATE = rand;
    return rand;
}

// Microsoft
function ms_seed(seed) {
    MS_STATE = seed || 0; // plain int
    return seed;
}

function ms() {
    let seed = MS_STATE || 0; // plain int
    let rand = ((214013*seed + 2531011) % 2**31);
    MS_STATE = rand;
    
    return Math.floor(rand / 2**16);
}


//
// general LCG version
//
// BigInt must be used.
function lcg_init(type,seed,multiplier,adder,mod,output_divisor) {
    LCG_HASH[type] = {
        type,
        seed: BigInt(seed),
        multiplier,
        adder,
        mod,
        output_divisor
    };
    return LCG_HASH[type];
}

function lcg(type) {
    let h = LCG_HASH[type];
    if (h === undefined) {
        throw new Error(`${type} is an unknown LCG type`);
    }
    let rand = ((h.multiplier*h.seed + h.adder) % h.mod);
    h.seed = rand;
    return rand / h.output_divisor;
}

console.log("bsd:", range(10).map(i=>bsd()));
bsd_seed(1n);
console.log("bsd seed(1):", range(10).map(i=>bsd()));

console.log("ms:", range(10).map(i=>ms()));
ms_seed(1);
console.log("ms seed(1):", range(10).map(i=>ms()));

console.log("\nlcg as bsd:");
lcg_init("bsd",0,1103515245n,12345n,2n**31n,1n);
console.log("lcg(bsd) seed 0", range(10).map(i=>lcg("bsd")));

lcg_init("bsd",1,1103515245n,12345n,2n**31n,1n);
console.log("lcg(bsd) seed 1", range(10).map(i=>lcg("bsd")));

console.log("\nlcg (as ms):");
lcg_init("ms",0,214013n,2531011n,2n**31n,2n**16n);
console.log("lcg(ms) seed 0", range(10).map(i=>lcg("ms")));

lcg_init("ms",1,214013n,2531011n,2n**31n,2n**16n);
console.log("lcg(ms) seed 1", range(10).map(i=>lcg("ms")));

try {
    console.log("lcg(unknown)", lcg("unknown"));
} catch(e) {
    console.log(e);
}

