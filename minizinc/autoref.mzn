% 
% Autoref problem in MiniZinc.
% 
% From Global constraint catalog
% http://www.emn.fr/z-info/sdemasse/gccat/Kautoref.html
% """
% A constraint that allows for modelling the autoref problem with one single constraint. 
% The autoref problem is a generalisation of the problem of finding a magic serie 
% and can be defined in the following way. Given an integer n > 0 and an integer 
% m >= 0, the problem is to find a non-empty finite series S=(s0,s1,...,sn,sn+1) 
% such that (1) there are si occurrences of i in S for each integer i ranging 
% from 0 to n, and (2) sn+1=m. This leads to the following model:
% 
% global_cardinality(
%  <var-s0,var-s1,...,var-sn,var-m>,
%  val-0 noccurrence-s0
%  val-1 noccurrence-s1,
%  < ... >
%  val-n noccurrence-sn
% )
% 
%
% 23, 2, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
% and 
% 23, 3, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 5 
% are the two unique solutions for n=27 and m=5.
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n = 27;
int: m = 5;
array[0..n+1] of var 0..n+1: s;

% solve satisfy;
solve :: int_search(s, first_fail, indomain_middle, complete) satisfy;

constraint

  % forall(i in 0..n) (
  %   s[i] = sum(j in 0..n) (bool2int(s[j] = i))
  % ) % :: domain
  % /\
  % s[n+1] = m

  let {
       array[0..n] of var 0..n: t = array1d(0..n, [s[i] | i in 0..n])
  } in
  global_cardinality(s,array1d(0..n,set2array(index_set(t))), t) :: domain % bounds
  /\ s[n+1] = m
;

output [
       show(s), "\n"
];



