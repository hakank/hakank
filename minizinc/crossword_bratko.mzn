% 
% Crossword in in MiniZinc.
% 
% This version was inspired by Ivan Bratko's Prolog solution
% in his "Prolog Programming for Artificial Intelligence", 
% (4th edition), page 27f 
% 
% The problem is to fill this crossword with words:
%    
%    L1   L2    L3   L4    L5   XXX
%    L6   XXX   L7   XXX   L8   XXX
%    L9   L10   L11  L12   L13  L14
%    L15  XXX   XXX  XXX   L16  XXX
%
% Where the L* are letters to be identified.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

int: num_letters = 26;
array[1..num_letters] of string: letters = 
       ["a","b","c","d","e","f","g","h","i","j","k","l","m",
        "n","o","p","q","r","s","t","u","v","w","x","y","z"];


int: num_words3;
array[1..num_words3, 1..3] of int: words3;

int: num_words4;
array[1..num_words4, 1..4] of int: words4;

int: num_words5;
array[1..num_words5, 1..5] of int: words5;

int: num_words6;
array[1..num_words6, 1..6] of int: words6;

int: rows = 4;
int: cols = 6;
array[1..rows, 1..cols] of int: problem;

% number of letters to assign
int: N = 16;


% decision variables
array[0..N] of var 1..num_letters: L;



% solve satisfy;
solve :: int_search(
        L, 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

constraint
   table([L[1],L[2],L[3],L[4],L[5]], words5)
   /\
   table([L[9],L[10],L[11],L[12],L[13],L[14]], words6)
   /\

   table([L[1],L[6],L[9],L[15]], words4)
   /\
   table([L[3],L[7],L[11]], words3)
   /\
   table([L[5],L[8],L[13],L[16]], words4)
;


constraint
   forall(I,J in 1..num_segments where I < J) (
      not(forall(K in 1..max_length) (
          L[segments[I,K]] = L[segments[J,K]]
      ))
   )
;

% handle dummy variable 0
constraint
  L[0] = 1
;

%
% data
%
int: max_length = 8;
int: num_segments = 6;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
   1, 2, 3, 4, 5, 0, % row segments
   6, 0, 7, 0, 8, 0,
   9,10,11,12,13,14,
   15,0, 0, 0,16, 0,

   1, 6, 9,15, 0, 0, % column segments
   3, 7,11, 0, 0, 0,
   5, 8,13,16, 0, 0,
   0, 0,14, 0, 0, 0
]);


num_words3 = 3;
words3 = array2d(1..num_words3, 1..3,
    [d,o,g,
     r,u,n,
     t,o,p
    ]);

num_words4 = 5;
words4 = array2d(1..num_words4, 1..4,
    [
     f,i,v,e,
     f,o,u,r,
     l,o,s,t,
     m,e,s,s,
     u,n,i,t,
    ]);

num_words5 = 4;
words5 = array2d(1..num_words5, 1..5,
    [
     b,a,k,e,r,
     f,o,r,u,m,
     g,r,e,e,n,
     s,u,p,e,r,
    ]);

num_words6 = 4;
words6 = array2d(1..num_words6, 1..6,
    [
    p,r,o,l,o,g,
    v,a,n,i,s,h,
    w,o,n,d,e,r,
    y,e,l,l,o,w
    ]);



%
% the problem
%
problem = array2d(1..rows, 1..cols,
   [
     1,   2,   3,   4,   5,   0,
     6,   0,   7,   0,   8,   0,
     9,  10,  11,  12,  13,  14,
    15,   0,   0,   0,  16,   0
   ]);


output [
  show(L) ++ "\n"
] ++
[
  show(letters[fix(L[I])]) ++ " "
  | I in 1..N
]
++
["\n\n"] 
++
[
    if problem[i,j] = 0 then 
       "_"
    else 
       show(letters[fix(L[problem[i,j]])]) 
    endif
    ++ if j = cols then "\n" else " " endif 
  | i in 1..rows, j in 1..cols
]
++ ["\n"]
;


