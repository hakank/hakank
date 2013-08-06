% 
% Enigma puzzle 1396: Planets in MiniZinc.
%
% Problem formulation from 
% http://www.f1compiler.com/samples/Enigma%201396.f1.html
% """
%  Enigma 1396 W. Haigh, New Scientist magazine, 17 June 2006.
%
%  I have allocated distinct positive integers to the letters of the alphabet.
%  By adding up the values of the letters in their names, I have obtained the
%  following scores for some members of the  solar system:
%  PLUTO 40, URANUS 36, NEPTUNE 29, SATURN 33, JUPITER 50, MARS 32, EARTH 31,
%  MOON 36, VENUS 39, MERCURY 33, SUN 18.
%  
%  Please send in the value of PLANETS.
% 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

set of int: S = 1..26;

var S: p;
var S: l;
var S: u;
var S: t;
var S: o;
var S: r;
var S: a;
var S: n;
var S: s;
var S: e;
var S: i;
var S: m;
var S: j;
var S: h;
var S: v;
var S: c;
var S: y;
array[1..17] of var S: letters = [p,l,u,t,o,r,a,n,s,e,i,m,j,h,v,c,y];
var int: planets;



% solve satisfy;
solve :: int_search(letters, first_fail, indomain_min, complete) satisfy;

constraint
    all_different(letters) /\
    p + l + u + t + o = 40 /\
    u + r + a + n + u + s = 36 /\
    n + e + p + t + u + n + e = 29 /\
    s + a + t + u + r + n = 33 /\
    j + u + p + i + t + e + r = 50 /\
    m + a + r + s = 32 /\
    e + a + r + t + h = 31 /\
    m + o + o + n = 36 /\
    v + e + n + u + s = 39 /\
    m + e + r + c + u + r + y = 33 /\
    s + u + n = 18 /\
    p + l + a + n + e + t + s = planets

;


output [
  "[p, l, u, t, o, r, a, n, s, e, i, m, j, h, v, c, y]", "\n",
  show(letters), "\n",
  "planets: ", show(planets), "\n"
];