% 
% Relative sizes puzzle (Enigma 1515) in MiniZinc.
% 
% From http://www.f1compiler.com/samples/Enigma%201515.f1.html
% """
% Enigma 1515 Albert Hadded, New Scientist magazine, October 11, 2008.
%
% In the following statements, whole numbers are expressed in words written
% in capital letters, where different letters stand for different digits, the
% same letter consistently stands for the same digit, and leading digits 
% cannot be zero.
% If the total of four BEES and a SNAIL is greater than the total of eight 
% FLEAS and an ANT; and if the total of 18 ANTS and a GNAT is greater than
% the total of 18 FLIES and a FLEA, find the value of BEETLES.
%
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:%www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: Digit = 0..9;

var Digit: a;
var Digit: b;
var Digit: e;
var Digit: f;
var Digit: g;
var Digit: i;
var Digit: l;
var Digit: n;
var Digit: s;
var Digit: t;

var int: bees;
var int: snail;
var int: flea;
var int: fleas;
var int: flies;
var int: beetles;
var int: ant;
var int: ants;
var int: gnat;


solve satisfy;

constraint
    all_different([a,b,e,f,g,i,l,n,s,t]) /\

    bees =          b*1000+e*100+e*10+s /\ b != 0 /\
    snail = s*10000+n*1000+a*100+i*10+l /\ s != 0 /\
    fleas = f*10000+l*1000+e*100+a*10+s /\ f != 0 /\
    ant =                  a*100+n*10+t /\ a != 0 /\
 
    4*bees + snail > 8*fleas + ant /\

    ants =          a*1000+n*100+t*10+s /\ a != 0 /\
    gnat  =         g*1000+n*100+a*10+t /\ g != 0 /\
    flies = f*10000+l*1000+i*100+e*10+s /\ f != 0 /\
    flea =          f*1000+l*100+e*10+a /\ f != 0 /\
   
    18*ants + gnat > 18*flies + flea /\

    beetles = b*1000000+e*100000+e*10000+t*1000+l*100+e*10+s
;


output [
  "beetles: ", show(beetles), "\n",

];