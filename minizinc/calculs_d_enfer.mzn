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
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Gecode/fz gives the following solution in < 1 second
% with occurrence / indomain_max
% a_max: 16
% A: [12, 10, 8, 6, -10, 4, 13, -1, -3, 5, 3, 16, 1, 11, 0, -2, -4, 15, 2, 9, -15, 14, -7, 7, -6, -5]
% 
% The same solution is given by ECLiPSe/ic in 2.2 seconds

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

% The objective is to minimize the maximum of the absolute values of A[i]
array[1..N] of var R: A_abs;
var 0..N: a_max;


% solve satisfy;
% solve minimize a_max;
% solve :: int_search(A ++ A_abs ,occurrence, indomain_max, complete) minimize a_max;   % quite good
solve :: int_search(A , first_fail, indomain_max, complete) minimize a_max; % faster


constraint
  forall(i in 1..N) ( A_abs[i] = abs(A[i]) )
  /\
  maximum(a_max, A_abs)
  /\

  all_different(A)
  /\
  z+e+r+o = 0
  /\
  o+n+e = 1
  /\  
  t+w+o = 2
  /\
  t+h+r+e+e = 3
  /\
  f+o+u+r = 4
  /\
  f+i+v+e = 5
  /\
  s+i+x = 6
  /\
  s+e+v+e+n = 7
  /\
  e+i+g+h+t = 8
  /\
  n+i+n+e = 9
  /\
  t+e+n = 10
  /\
  e+l+e+v+e+n = 11
  /\
  t+w+e+l+f = 12


% /\ 
% a_max <= 16

 % the NCL solution
% /\
% A= [-16, -14, -13, -12, -10, 4, 13, -1, -3, -11, -9, 16, -8,  11,  0,  -6, -4, 15, 2, 9, -15, 14, -7, 7, -2, -5]


;

output [
  "a_max: ", show(a_max), "\n",
  "A: ", show(A), "\n",
%  "A_abs: ", show(A_abs), "\n",

];