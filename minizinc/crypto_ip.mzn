% 
% Crypto problem, integer programming model in MiniZinc.
% 
% This is an integer programming model of the standard benchmark "crypto" 
% problem.
%
% It is modelled after GLPK:s model cryto.mod.
%
% """
%  This problem comes from the newsgroup rec.puzzle.
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


% Note: This IP model is quite slow for the MiniZinc IP solvers:
%   - ECLiPSe eplex: 14.302 sec
%   - MiniZinc --mip: 4.898
%
% Compare with the traditional constraint programming version crypto.mzn which
% solves the problem in < 1 sec by MiniZinc/flatzinc, Gecode/fz, 
% ECLiPSe ic|fd  .

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%



include "globals.mzn"; 

% set of letters
int: num_letters = 26;
set of int: letters = 1..num_letters; 

int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;
int: G = 7;
int: H = 8;
int: I = 9;
int: J = 10;
int: K = 11;
int: L = 12;
int: M = 13;
int: N = 14;
int: O = 15;
int: P = 16;
int: Q = 17;
int: R = 18;
int: S = 19;
int: T = 20;
int: U = 21;
int: V = 22;
int: W = 23;
int: X = 24;
int: Y = 25;
int: Z = 26;

array[1..num_letters] of letters: all_letters = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z];
array[1..num_letters] of var letters: letters_res;

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

%  set of values assigned to the letters 
set of int: values = 1..num_letters;

% x[i,j] = 1 means that letter i is assigned value j 
array[letters, values] of var 0..1: x;


%
% Converts x to a list of the letter values
%
predicate binmatrix2num_ip(array[int,int] of var int: x, array[int] of var int: nums) =

   forall(i in index_set_1of2(x)) (
      nums[i] =  sum(j in index_set_2of2(x)) (j*x[i,j])
   )
;


% Note: x is global.
predicate sum_letters(array[int] of letters: a, int: word) =
       word = sum(k in 1..length(a), j in values) (
          j * x[a[k],j]
       )
;

% solve satisfy;
solve :: int_search([x[i,j] | i in letters, j in values], first_fail, indomain_median, complete) satisfy;


constraint  forall(i in letters) (sum(j in values) (x[i,j]) = 1);
constraint forall(j in values)  (sum(i in letters) (x[i,j]) = 1);

% The GLPK Math Prog solutions is quite elegant:
% s.t. eqn{word in WORDS}: sum{k in 1..length(word), j in values}
%     j * x[substr(word,k,1), j] = total[word];
        
% Unfortunately MiniZinc don't handle strings and thus cannot split
% strings, so here we go.
% MiniZinc do, however, allows definining predicates which makes it quite 
% easy.
constraint   sum_letters([B,A,L,L,E,T],       BALLET);
constraint   sum_letters([C,E,L,L,O],         CELLO)     ;
constraint   sum_letters([C,O,N,C,E,R,T],     CONCERT)   ; 
constraint   sum_letters([F,L,U,T,E],         FLUTE)     ; 
constraint   sum_letters([F,U,G,U,E],         FUGUE)     ; 
constraint   sum_letters([G,L,E,E],           GLEE)      ; 
constraint   sum_letters([J,A,Z,Z],           JAZZ)      ; 
constraint   sum_letters([L,Y,R,E],           LYRE)      ; 
constraint   sum_letters([O,B,O,E],           OBOE)      ; 
constraint   sum_letters([O,P,E,R,A],         OPERA)     ; 
constraint   sum_letters([P,O,L,K,A],         POLKA)     ; 
constraint   sum_letters([Q,U,A,R,T,E,T],     QUARTET)   ; 
constraint   sum_letters([S,A,X,O,P,H,O,N,E], SAXOPHONE) ; 
constraint   sum_letters([S,C,A,L,E],         SCALE)     ; 
constraint   sum_letters([S,O,L,O],           SOLO)      ; 
constraint   sum_letters([S,O,N,G],           SONG)      ; 
constraint   sum_letters([S,O,P,R,A,N,O],     SOPRANO)   ; 
constraint   sum_letters([T,H,E,M,E],         THEME)     ; 
constraint   sum_letters([V,I,O,L,I,N],       VIOLIN)    ; 
constraint   sum_letters([W,A,L,T,Z],         WALTZ) ; 

constraint
   binmatrix2num_ip(x, letters_res)

;

% printf{i in letters} "  %s", i;
% printf "\n";

% printf{i in letters} " %2d", sum{j in values} j * x[i,j];
% printf "\n";

output [
  "letters: ", show(letters_res),"\n"
   
];