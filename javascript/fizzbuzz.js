/* 

  FizzBuzz in JavaScript.

  http://rosettacode.org/wiki/FizzBuzz
  """
  Task

  Write a program that prints the integers from 1 to 100  (inclusive).

  But:

  - for multiples of three, print Fizz (instead of the number)
  - for multiples of five, print Buzz (instead of the number)
  - for multiples of both three and five, print FizzBuzz (instead of the number) 


  The FizzBuzz problem was presented as the lowest level of comprehension 
  required to illustrate adequacy.
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
const {partition,dec2base,transpose,range2,sum2} = require('./js_utils.js');

function fizzbuzz0() {
    console.log("fizzbuzz0");
    let res = [];
    for(let n = 1; n <= 100; n++) {
        if (n % 3 === 0 && n % 5 === 0) {
            res.push('FizzBuzz');
        } else if (n % 3 === 0) {
            res.push('Fizz');            
        } else if (n % 5 === 0) {
            res.push('Buzz');
        } else {
            res.push(n);
        }
    }
    console.log(res);
}


function fizzbuzz1() {
    console.log("fizzbuzz1");    
    console.log(range2(1,100)
                .map(n=>
                     (n % 3 === 0 && n % 5 === 0) ?
                     'FizzBuzz' :
                     (n % 3 === 0) ?
                     'Fizz' :
                     (n % 5 === 0) ?
                     'Buzz' : n
                    ));
}

function fizzbuzz2() {
    console.log("fizzbuzz2");
    const to100 = range2(1,100);
    let mod3 = to100.map(n=> n % 3 === 0 ? 'Fizz' : n);
    let mod5 = to100.map(n=> n % 5 === 0 ? 'Buzz' : n);
    let mod3_and_5 = to100.map(n=> n % 3 === 0 && n % 5 === 0 ? 'FizzBuzz' : n);
    console.log(transpose([mod3,mod5,mod3_and_5])
                .map(e=>e.sort()[2])
               );
}

// It gets sillier and sillier...
function fizzbuzz3() {
    console.log("fizzbuzz3");
    function t(n) {
        return range2(1,Math.floor(100/n)).map(i=>i*n) 
    }
    let to100 = range2(1,100);
    t(3).map(e=>to100[e-1] = 'Fizz');
    t(5).map(e=>to100[e-1] = 'Buzz');
    t(15).map(e=>to100[e-1] = 'FizzBuzz');
    console.log(to100);
}


function fizzbuzz4() {
    console.log("fizzbuzz4");
    const h = {
        3: "Fizz",
        5: "Buzz",
        15: "FizzBuzz"
    };
    const a = [3,5,15];
    console.log(range2(1,100)
                .map(n=>a.map(i=>n % i === 0 ? h[i] : n).sort()[2]));
}


function fizzbuzz5() {
    console.log("fizzbuzz5");
    const f = [0,0,"Fizz",0,"Buzz","Fizz",0,0,"Fizz","Buzz",0,"Fizz",0,0,"FizzBuzz"];
    const ff = range2(1,Math.floor(100/f.length)+1).map(i=>f).flatten2();
    console.log(range2(1,100).map((e,i)=>ff[i]===0 ? i+1 : ff[i]));
}

function fizzbuzz6() {
    console.log("fizzbuzz6");
    console.log(
        range2(1,100)
            .map(n=>
                 [n,"Fizz","Buzz","FizzBuzz"][[3,5].map((e,i)=>(i+1)*(n%e===0)).sum2()])
    );
}

function fizzbuzz7() {
    console.log("fizzbuzz7");
    let j=(j)=>j.join(""),s=(s)=>s.split(""),r=(r)=>r.reverse(),srj=(q)=>eval(j(r(s(q))));
    srj(';)ser(gol.elosnoc}};)n(hsup.ser{esle};)"zzuB"(hsup.ser{)0===5%n(fi esle};)"zziF"(hsup.ser{)0===3%n(fi esle};)"zzuBzziF"(hsup.ser{)0===5%n&&0===3%n(fi{)++n;001=<n;1=n tel(rof;][=ser tel');

}

function fizzbuzz8() {
    console.log("fizzbuzz8");

    Number.prototype.fizzbuzz = function() {
        if (this % 15 === 0) {
            return "FizzBuzz";
        } else if (this % 5 === 0) {
            return "Buzz";            
        } else if (this % 3 === 0) {
            return "Fizz";
        } else {
            return this;
        }
    }

    console.log(range2(1,100)
                .map(n=>n.fizzbuzz()));

}

// Oh, we must have an OO version!
function fizzbuzz9() {
    console.log("fizzbuzz9");

    class FizzBuzz {
        constructor(n) { this.n = n;                                   }
        fizz()         { return !(this.n % 3)  ? "Fizz" : "";        }
        buzz()         { return !(this.n % 5)  ? "Buzz" : "";        }
        fizz_buzz()    { return (this.fizz() + this.buzz()) || this.n; }
    }
    
    console.log(range2(1,100)
                .map(n=>(new FizzBuzz(n)).fizz_buzz())
               );
    
}

function fizzbuzz10() {
    console.log("fizzbuzz10");

    function* fb10() {
        let n = 1;
        while(n <= 100) {
            if (n % 15 === 0) {
                yield "FizzBuzz";
            } else if (n % 5 === 0) {
                yield "Buzz";
            } else if (n % 3 === 0) {
                yield "Fizz";
            } else {
                yield n;
            }
            n++;
        }
    }

    const fb10f = fb10();
    let f = fb10f;
    let ret = [];
    while(true) {
        f = fb10f.next();
        if (f.done) {
            break;
        }
        ret.push(f.value);
    }
    console.log(ret);
    
}

function fizzbuzz11() {
    console.log("fizzbuzz11");
    console.log(range2(1,100).map(n=>(!(n%3)?"Fizz":"")+(!(n%5)?"Buzz":"")||n));
}


function fizzbuzz12() {
    console.log("fizzbuzz12");
    console.log(
        "..-.+-..-+.-..*..-.+-..-+.-..*..-.+-..-+.-..*..-.+-..-+.-..*..-.+-..-+.-..*..-.+-..-+.-..*..-.+-..-+"
            .split("")
            .map((n,i)=>n.replace(/\*/g,"FizzBuzz").replace(/\-/g,"Fizz").replace(/\+/g,"Buzz").replace(/\./g,i+1))
            .map(n=>/\d+/.test(n) ? parseInt(n) : n)
    )
}


fizzbuzz0();
fizzbuzz1();
fizzbuzz2();
fizzbuzz3();
fizzbuzz4();
fizzbuzz5();
fizzbuzz6();
fizzbuzz7();
fizzbuzz8();
fizzbuzz9();
fizzbuzz10();
fizzbuzz11();
fizzbuzz12();

