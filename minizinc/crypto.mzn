% 
% Crypto problem in MiniZinc.
% 
% This is the standard benchmark "crypto" problem.
%
% From GLPK:s model cryto.mod.
%
% """
%    This problem comes from the newsgroup rec.puzzle.
%    The numbers from 1 to 26 are assigned to the letters of the alphabet.
%    The numbers beside each word are the total of the values assigned to
%    the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
%    5, 9, 20 and 13, or any other combination that add up to 47).
%    Find the value of each letter under the equations:
%
%    BALLET  45     GLEE  66     POLKA      59     SONG     61
%    CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
%    CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
%    FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
%    FUGUE   50     OPERA 65     SOLO       37     WALTZ    34
%
%    Solution:
%    A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
%    5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18
%
%    Reference:
%    Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
%    Simple problems, the crypto-arithmetic puzzle ALPHACIPHER. */
% """

% Compare with the integer programming model crypto_io.mzn.

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% set of letters
int: num_letters = 26;
set of int: LETTERS = 1..num_letters; 

var LETTERS: A;
var LETTERS: B;
var LETTERS: C;
var LETTERS: D;
var LETTERS: E;
var LETTERS: F;
var LETTERS: G;
var LETTERS: H;
var LETTERS: I;
var LETTERS: J;
var LETTERS: K;
var LETTERS: L;
var LETTERS: M;
var LETTERS: N;
var LETTERS: O;
var LETTERS: P;
var LETTERS: Q;
var LETTERS: R;
var LETTERS: S;
var LETTERS: T;
var LETTERS: U;
var LETTERS: V;
var LETTERS: W;
var LETTERS: X;
var LETTERS: Y;
var LETTERS: Z;

array[1..num_letters] of var LETTERS: all_letters = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z];

int:  BALLET     =  45;
int:  CELLO      =  43;
int:  CONCERT    =  74;
int:  FLUTE      =  30;
int:  FUGUE      =  50;
int:  GLEE       =  66;
int:  JAZZ       =  58;
int:  LYRE       =  47;
int:  OBOE       =  53;
int:  OPERA      =  65;
int:  POLKA      =  59;
int:  QUARTET    =  50;
int:  SAXOPHONE  = 134;
int:  SCALE      =  51;
int:  SOLO       =  37;
int:  SONG       =  61;
int:  SOPRANO    =  82;
int:  THEME      =  72;
int:  VIOLIN     = 100;
int:  WALTZ      =  34;


% solve satisfy;
solve :: int_search(all_letters, first_fail, indomain_median, complete) satisfy;


constraint
   all_different(all_letters)
   /\
   sum([B,A,L,L,E,T])       = BALLET    /\
   sum([C,E,L,L,O])         = CELLO     /\
   sum([C,O,N,C,E,R,T])     = CONCERT   /\ 
   sum([F,L,U,T,E])         = FLUTE     /\ 
   sum([F,U,G,U,E])         = FUGUE     /\ 
   sum([G,L,E,E])           = GLEE      /\ 
   sum([J,A,Z,Z])           = JAZZ      /\ 
   sum([L,Y,R,E])           = LYRE      /\ 
   sum([O,B,O,E])           = OBOE      /\ 
   sum([O,P,E,R,A])         = OPERA     /\ 
   sum([P,O,L,K,A])         = POLKA     /\ 
   sum([Q,U,A,R,T,E,T])     = QUARTET   /\ 
   sum([S,A,X,O,P,H,O,N,E]) = SAXOPHONE /\ 
   sum([S,C,A,L,E])         = SCALE     /\ 
   sum([S,O,L,O])           = SOLO      /\ 
   sum([S,O,N,G])           = SONG      /\ 
   sum([S,O,P,R,A,N,O])     = SOPRANO   /\ 
   sum([T,H,E,M,E])         = THEME     /\ 
   sum([V,I,O,L,I,N])       = VIOLIN    /\ 
   sum([W,A,L,T,Z])         = WALTZ 

;

output [
  "letters: ", show(all_letters), "\n",
   
];