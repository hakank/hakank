%
% "Einstein puzzle" in Minizinc 
% 
% Translated from the LPL model in
% Tony Hurlimann "Mathematical Modelling of Puzzles and Games", page 181
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

set of 1..5: a = 1..5; % Nationalities: Norwegian, Dane, Briton, Swede, German
set of 1..5: b = 1..5; % Colors: yellow blue red green white
set of 1..5: c = 1..5; % Pets: cat bird dog horse fish
set of 1..5: d = 1..5; % Drinks: coffee tea milk juice water
set of 1..5: e = 1..5; % Smokes: Dunhill Marlboro Pall-Mall Bluemaster Prince

array[a] of var a: A;
array[b] of var b: B;
array[c] of var c: C;
array[d] of var d: D;
array[e] of var e: E;

array[a] of string: nationalities = ["Norwegian", "Dane", "Briton", "Swede", "German"];

solve satisfy;

constraint
    all_different(A) /\
    all_different(B) /\
    all_different(C) /\
    all_different(D) /\
    all_different(E) /\

    A[3] = B[3] /\ 
    A[4] = C[3] /\ 
    A[2] = D[2] /\ 
    B[4] + 1 = B[5] /\ 
    B[4] = D[1] /\ 
    E[3] = C[2] /\ 
    D[3] = 3 /\ 
    B[1] = E[1] /\ 
    A[1] = 1 /\ 
    (E[2] = C[1]+1 \/ E[2] = C[1]-1) /\ 
    (C[4] = E[1]+1 \/ C[4] = E[1]-1) /\ 
    E[4] = D[4] /\ 
    B[2] = 2 /\ 
    A[5] = E[5] /\ 
    (E[2] = D[5] + 1 \/ E[2] = D[5] - 1)
;


output [
   "A: ", show(A), "\n",
   "B: ", show(B), "\n",
   "C: ", show(C), "\n",
   "D: ", show(D), "\n",
   "E: ", show(E), "\n"
] ++
[
  "The " ++ show(nationalities[fix(A[C[5]])]) ++ " owns the fish\n"
]++["\n"];
