% 
% Calculs d'enfer puzzle in MiniZinc.
% 
% Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
% http://citeseer.ist.psu.edu/161721.html
% 
% The solution is the manual is:
% """
% a = -16, b = -14, c = -13, d = -12, e = -10,
% f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
% l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
% r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
% x = 7, y = -2, z = -5.
%
% max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
% """
%
% Also, see the discussion of the Z model:
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
% (which shows the same solution).
%
%
% Alternative model to http://hakank.org/minizinc/calculs_d_enfer.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: N = 26;
set of int: R = -100..100;

var R: a;
var R: b;
var R: c;
var R: d;
var R: e;
var R: f;
var R: g;
var R: h;
var R: i;
var R: j;
var R: k;
var R: l;
var R: m;
var R: n;
var R: o;
var R: p;
var R: q;
var R: r;
var R: s;
var R: t;
var R: u;
var R: v;
var R: w;
var R: x;
var R: y;
var R: z;
array[1..N] of var R: A = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z];

array[0..12,1..6] of var int: words = 
array2d(0..12,1..6,
[
  0,0,z,e,r,o,
  0,0,0,o,n,e,
  0,0,0,t,w,o,
  0,t,h,r,e,e,
  0,0,f,o,u,r,
  0,0,f,i,v,e,
  0,0,0,s,i,x,
  0,s,e,v,e,n,
  0,e,i,g,h,t,
  0,0,n,i,n,e,
  0,0,0,t,e,n,
  e,l,e,v,e,n,
  0,t,w,e,l,f,
]);


% The objective is to minimize the maximum of the absolute values of A[i]
array[1..N] of var R: A_abs;
var 0..N: a_max;



% solve satisfy;
% solve minimize a_max;
solve :: int_search(A, first_fail, indomain_min, complete) minimize a_max; % faster

constraint
  forall(I in 1..N) ( A_abs[I] = abs(A[I]) )
  /\
  maximum(a_max, A_abs)
  /\
  all_different(A) 
  /\
  forall(W in 0..12) (
     sum([words[W,I] | I in 1..6]) = W
  )


;

output [
  "a_max: ", show(a_max), "\n",
  "A: ", show(A), "\n",
%  "A_abs: ", show(A_abs), "\n",

];