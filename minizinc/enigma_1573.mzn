% 
% Enigma Number 1573 (Sat, Sun, Mon, Tue) in MiniZinc.
% 
% From New Scientist
% http://www.newscientist.com/article/mg20427360.800-enigma-number-1573.html
% """
% 25 November 2009 by Albert Haddad
%
% Sat, Sun, Mon, Tue
% 
% Four consecutive days are noted in the two multiplication sums shown 
% on the right, where different letters stand for different digits, the 
% same letter stands for the same digit, each dot can be any digit, 
% and leading digits cannot be zero.
% 
% Assuming that a MONTH is exactly divisible by each of 28, 30 and 31, 
% what is the numerical total of the two five-digit products?
% 
% [The figure is
%     S A T          M O N
%   *   . .        *   . .  
%   -------        -------
%   . . . .        . . . .
% . . . .        . . . . 
% ---------      ---------
% S U N . .      T U E . . 
% ]
% """

%
% Note: I first send in a solution, and then I changed the model slightly 
%       which now finds two solutions (of my first is one).
%       These two solutions finds the same MONTH but the two 5-digit numbers
%       are different, and hence the total.
%
% The founds solutions are:
% a) This is the one I send to New Scientist
%
%    [S, A, T, M, O, N, U, E]
% x: [3, 5, 2, 7, 8, 1, 4, 9]
% SAT: 352
% MON: 781
% MONTH = 78120
% SUNab = 34144
% TUEcd = 24992
% total = 59136
% 
% 
% b)
%    [S, A, T, M, O, N, U, E]
% x: [4, 6, 2, 7, 8, 1, 0, 3]
% SAT: 462
% MON: 781
% MONTH = 78120
% SUNab = 40194
% TUEcd = 20306
% total = 60500

% Note that MONTH is the same in the two solutions (as is H, namely 0).



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
set of int: D = 0..9;
var D: S;
var D: A;
var D: T;
var D: M;
var D: O;
var D: N;
var D: U;
var D: E;

% This could be anything (happens to be 0)
var D: H;

% Tmp variables: 
%   a and b is the last two digits in SUNab
%   c and d is the last two digits in TUEcd
var D: a; 
var D: b = SAT1 mod 10; % b is also the last digit in SAT1
var D: c;
var D: d = MON1 mod 10; % d is also the last digit in MON1

var 100..999: SAT = 100*S + 10*A + T;
var 10..99: SAT_tmp;
% SAT1 = SAT * last digit in SAT_tmp
% SAT2 = SAT * first digit in SAT_tmp
var 1000..9999: SAT1 = SAT*(SAT_tmp mod 10);
var 1000..9999: SAT2 = SAT*(SAT_tmp div 10);

var 100..999: MON = 100*M + 10*O + N;
var 10..99: MON_tmp;
% MON1 = MON * last digit in MON_tmp
% MON2 = MON * first digit in MON_tmp
var 1000..9999: MON1 = MON*(MON_tmp mod 10);
var 1000..9999: MON2 = MON*(MON_tmp div 10);

var 10000..99999: SUNab = 10000*S + 1000*U + 100*N + 10*a + b;
var 10000..99999: TUEcd = 10000*T + 1000*U + 100*E + 10*c + d;

var int: MONTH = 10000*M + 1000*O + 100*N + 10*T + H;
var int: total = SUNab + TUEcd;

array[1..8] of var D: x = [S,A,T,M,O,N,U,E];


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x)  /\
 
  S > 0 /\ T > 0 /\

  SUNab = SAT * SAT_tmp  /\
  TUEcd = MON * MON_tmp  /\

  SUNab = SAT1 + SAT2*10 /\ 
  TUEcd = MON1 + MON2*10 /\ 

  MONTH mod 28 = 0 /\
  MONTH mod 30 = 0 /\
  MONTH mod 31 = 0 
;


output [
  "S: " ++ show(S) ++ "\n" ++
  "A: " ++ show(A) ++ "\n" ++
  "T: " ++ show(T) ++ "\n" ++
  "M: " ++ show(M) ++ "\n" ++
  "O: " ++ show(O) ++ "\n" ++
  "N: " ++ show(N) ++ "\n" ++
  "U: " ++ show(U) ++ "\n" ++
  "E: " ++ show(E) ++ "\n" ++
  "H: " ++ show(H) ++ "\n" ++

  "a:" ++ show(a) ++ "\n" ++
  "b:" ++ show(b) ++ "\n" ++
  "c:" ++ show(c) ++ "\n" ++
  "d:" ++ show(d) ++ "\n" ++

  "SAT: " ++ show(SAT)  ++ "\n" ++
  "SAT_tmp: " ++ show(SAT_tmp)  ++ "\n" ++
  "SAT1: " ++ show(SAT1)  ++ "\n" ++
  "SAT2: " ++ show(SAT2)  ++ "\n" ++

  "MON: " ++ show(MON)  ++ "\n" ++
  "MON_tmp: " ++ show(MON_tmp)  ++ "\n" ++
  "MON1: " ++ show(MON1)  ++ "\n" ++
  "MON2: " ++ show(MON2)  ++ "\n" ++

  "SUNab: " ++ show(SUNab)  ++ "\n" ++
  "TUEcd: " ++ show(TUEcd)  ++ "\n" ++

  "MONTH: " ++ show(MONTH) ++ "\n" ++
  "total: " ++ show(total) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 
] ++ 
["\n"]
;