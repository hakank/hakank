/*
 From The OptimJ manual


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/


param g := 9.81;
var v0 >= 0 := 1;
var alpha >= 0 := 1;
var rangex >= 0 := 1;

s.t.
 c1: rangex = v0*v0 * sin(2 * alpha) / g;

maximize z: rangex;

# option solver bonmin;
option solver snopt;

solve;

display rangex, v0, alpha;

