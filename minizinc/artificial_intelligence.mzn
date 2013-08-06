% 
%   ... = ARTIFICIAL INTELLIGENCE in MiniZinc.
%
%              AN
%  + ACCELERATING
%   + INFERENTIAL
%   + ENGINEERING
%          + TALE
%         + ELITE
%         + GRANT
%           + FEE
%            + ET
%        + CETERA =
%   -------------
%      ARTIFICIAL
%  + INTELLIGENCE
% 
%  From Erwin Kalvelagen "Alphametics (3)"
%  http://yetanothermathprogrammingconsultant.blogspot.com/2009/04/alphametics-3.html
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: Domain = 0..9;
var Domain: A;
var Domain: N;
var Domain: C;
var Domain: E;
var Domain: L;
var Domain: R;
var Domain: T;
var Domain: I;
var Domain: G;
var Domain: F;

array[1..10] of var Domain: x = [A,N,C,E,L,R,T,I,G,F];

int: Te1 = ceil(pow(10.0, 1.0));
int: Te2 = ceil(pow(10.0, 2.0));
int: Te3 = ceil(pow(10.0, 3.0));
int: Te4 = ceil(pow(10.0, 4.0));
int: Te5 = ceil(pow(10.0, 5.0));
int: Te6 = ceil(pow(10.0, 6.0));
int: Te7 = ceil(pow(10.0, 7.0));
int: Te8 = ceil(pow(10.0, 8.0));
int: Te9 = ceil(pow(10.0, 9.0));
int: Te10 = ceil(pow(10.0, 10.0)); % This OVERFLOWS! (As in Comet)
int: Te11 = ceil(pow(10.0, 11.0));

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  all_different(x)
  /\
    Te1*A+N+
    Te11*A+Te10*C+Te9*C+Te8*E+Te7*L+Te6*E+Te5*R+Te4*A+Te3*T+100*I+10*N+G+
    Te10*I+Te9*N+Te8*F+Te7*E+Te6*R+Te5*E+Te4*N+Te3*T+100*I+10*A+L+
    Te10*E+Te9*N+Te8*G+Te7*I+Te6*N+Te5*E+Te4*E+Te3*R+100*I+10*N+G+
    Te3*T+100*A+10*L+E+
    Te4*E+Te3*L+100*I+10*T+E+
    Te4*G+Te3*R+100*A+10*N+T+
    100*F+10*E+E+
    10*E+T+
    Te5*C+Te4*E+Te3*T+100*E+10*R+A ==
    Te9*A+Te8*R+Te7*T+Te6*I+Te5*F+Te4*I+Te3*C+100*I+10*A+L+
    Te11*I+Te10*N+Te9*T+Te8*E+Te7*L+Te6*L+Te5*I+Te4*G+Te3*E+100*N+10*C+E
 ;

output[
   show(x), "\n",
];

