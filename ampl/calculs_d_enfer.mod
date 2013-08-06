/*

  Calculs d'enfer puzzle in AMPL+CP.

  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html
  
  The solution is the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.

  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """

  Also, see the discussion of the Z model:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  (which shows the same solution).


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters;

param n;

var A{letters} >= -100 <= 100 integer;
var A_abs{letters} >= 0 <= 100 integer;
var a_max >= 0 <= n integer;

minimize obj: a_max;

#
# constraints
#
s.t. c1: alldiff{i in letters} A[i];

s.t. c2{i in letters}: A_abs[i] = abs(A[i]);

#
# This constraintdon't work (as expected).
# s.t. c3: a_max = max(A_abs);
# so I roll my own max(array).
#
s.t. c3{j in letters}:
       a_max >= A_abs[j]
;

s.t. c4: 
  A['z']+A['e']+A['r']+A['o'] = 0
  and
  A['o']+A['n']+A['e'] = 1
  and  
  A['t']+A['w']+A['o'] = 2
  and
  A['t']+A['h']+A['r']+A['e']+A['e'] = 3
  and
  A['f']+A['o']+A['u']+A['r'] = 4
  and
  A['f']+A['i']+A['v']+A['e'] = 5
  and
  A['s']+A['i']+A['x'] = 6
  and
  A['s']+A['e']+A['v']+A['e']+A['n'] = 7
  and
  A['e']+A['i']+A['g']+A['h']+A['t'] = 8
  and
  A['n']+A['i']+A['n']+A['e'] = 9
  and
  A['t']+A['e']+A['n'] = 10
  and
  A['e']+A['l']+A['e']+A['v']+A['e']+A['n'] = 11
  and
  A['t']+A['w']+A['e']+A['l']+A['f'] = 12
;

data;

param n := 26;
set letters  := a b c d e f g h i j k l m n o p q r s t u v w x y z;

option solver gecode;
option gecode_options 'icl=def var_branching=degree_size_max val_branching=max outlev=1 outfreq=1';
# option solver ilogcp;

solve;

# display A_abs;
display a_max;
for{i in letters} {
   printf "%2d ", A[i];
}
printf "\n";