% 
% Designing a box in MiniZinc.
% 
% Design a box at minimum cost that meets
% area, volume, marketing and aesthetic requirements;
%
% AMPL:
% objective 50.96507524
% d  23.031
% w  9.5622
% h  6.86566

% JaCoP:
%  z: 50.94
%  d: 23.03
%  w: 9.56
%  h: 6.86


% Both Gecode and G12/mip:
%   Constraint float_div_reif not found

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

var float: d;
var float: w;
var float: h;

var float: z = 2.0*( 0.05*(d*w + d*h) + 0.1*w*h);

% solve minimize z;
solve :: float_search([z,d,w,h], 0.01, input_order,indomain_split,complete) minimize z;


constraint
  z >= 0.0 /\
  d >= 0.0 /\ 
  w >= 0.0 /\ 
  h >= 0.0
  /\
  2.0*(h*d + h*w + d*w) >= 888.0
  /\
  h*d*w >= 1512.0
  /\
  % These two enforce aesthetics;
  h/w <= 0.718
  /\
  h/w >= 0.518
  /\
  % Marketing requires a small footprint;
  d*w <= 252.0
;


output 
[
  "z: ", show(z), "\n", 
  "d: ", show(d), "\n", 
  "w: ", show(w), "\n", 
  "h: ", show(h), "\n", 

];