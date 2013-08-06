% 
% Benjamin Franklin's 8x8 Magic Square in MiniZinc.
% 
% Problem formulation from
% the Formula One model:
% http://www.f1compiler.com/samples/Franklin%27s%208x8%20Magic%20Square.f1.html
% """
% In 2006 in the article published online by the Proceedings of the Royal 
% Society "Enumerating the bent diagonal squares of Dr Benjamin Franklin FRS"
% by Daniel Schindel, Matthew Rempel and Peter Loly
% the authors showed there are 1,105,920 variations of his magic square.
% (http://www.physics.umanitoba.ca/news/loly_paper.pdf)
% """
%
% Also, see e.g.
% http://www.mathpages.com/home/kmath155.htm


%
% The following is a simple translation of the F1 model to MiniZinc.
% Note: I have not checked that this model generates exactly 
% 1,105,920 solutions.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 8;

int: Sum = 260;
int: Sum2 = Sum div 2;

set of int: r = 1..64;
var r:a1;var r:a2;var r:a3;var r:a4;var r:a5;var r:a6;var r:a7;var r:a8;
var r:b1;var r:b2;var r:b3;var r:b4;var r:b5;var r:b6;var r:b7;var r:b8;
var r:c1;var r:c2;var r:c3;var r:c4;var r:c5;var r:c6;var r:c7;var r:c8;
var r:d1;var r:d2;var r:d3;var r:d4;var r:d5;var r:d6;var r:d7;var r:d8;
var r:e1;var r:e2;var r:e3;var r:e4;var r:e5;var r:e6;var r:e7;var r:e8;
var r:f1;var r:f2;var r:f3;var r:f4;var r:f5;var r:f6;var r:f7;var r:f8;
var r:g1;var r:g2;var r:g3;var r:g4;var r:g5;var r:g6;var r:g7;var r:g8;
var r:h1;var r:h2;var r:h3;var r:h4;var r:h5;var r:h6;var r:h7;var r:h8;

array[1..n, 1..n] of var r: a;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 


% solve satisfy;
solve :: int_search([a[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint

    forall(i in 1..n) (
       all_different([a[i,j] | j in 1..n])
    )

    /\
    forall(j in 1..n) (
       all_different([a[i,j] | i in 1..n])
    )

    /\       
    % An array of 64 variables of an 8x8 square
    cp2d(a, array2d(1..n, 1..n, 
        [ a1, a2, a3, a4, a5, a6, a7, a8,
          b1, b2, b3, b4, b5, b6, b7, b8,
          c1, c2, c3, c4, c5, c6, c7, c8,
          d1, d2, d3, d4, d5, d6, d7, d8,
          e1, e2, e3, e4, e5, e6, e7, e8,
          f1, f2, f3, f4, f5, f6, f7, f8,
          g1, g2, g3, g4, g5, g6, g7, g8,
          h1, h2, h3, h4, h5, h6, h7, h8]))  /\ 

    % Constraints to to remove symmetries. These have nothing to do 
    % with the Benjamin Franklin's constraints.

    a1 > a8  /\  a1 > h1  /\  a1 > h8  /\  a8 > h1  /\       

    % The actual Benjamin Franklin's constraints for 8x8 Magic Square
    % Half rows and half columns (32 constraints)

    a1 + a2 + a3 + a4 = Sum2  /\   
    b1 + b2 + b3 + b4 = Sum2  /\   
    c1 + c2 + c3 + c4 = Sum2  /\   
    d1 + d2 + d3 + d4 = Sum2  /\   
    a1 + b1 + c1 + d1 = Sum2  /\   
    a2 + b2 + c2 + d2 = Sum2  /\   
    a3 + b3 + c3 + d3 = Sum2  /\   
    a4 + b4 + c4 + d4 = Sum2  /\   
    a5 + a6 + a7 + a8 = Sum2  /\   
    b5 + b6 + b7 + b8 = Sum2  /\   
    c5 + c6 + c7 + c8 = Sum2  /\   
    d5 + d6 + d7 + d8 = Sum2  /\   
    a5 + b5 + c5 + d5 = Sum2  /\   
    a6 + b6 + c6 + d6 = Sum2  /\   
    a7 + b7 + c7 + d7 = Sum2  /\   
    a8 + b8 + c8 + d8 = Sum2  /\   
    e1 + f1 + g1 + h1 = Sum2  /\   
    e2 + f2 + g2 + h2 = Sum2  /\   
    e3 + f3 + g3 + h3 = Sum2  /\   
    e4 + f4 + g4 + h4 = Sum2  /\   
    e1 + e2 + e3 + e4 = Sum2  /\   
    f1 + f2 + f3 + f4 = Sum2  /\   
    g1 + g2 + g3 + g4 = Sum2  /\   
    h1 + h2 + h3 + h4 = Sum2  /\   
    e5 + e6 + e7 + e8 = Sum2  /\   
    f5 + f6 + f7 + f8 = Sum2  /\   
    g5 + g6 + g7 + g8 = Sum2  /\   
    h5 + h6 + h7 + h8 = Sum2  /\   
    e5 + f5 + g5 + h5 = Sum2  /\   
    e6 + f6 + g6 + h6 = Sum2  /\   
    e7 + f7 + g7 + h7 = Sum2  /\   
    e8 + f8 + g8 + h8 = Sum2  /\   

    % Bent Diagonals (8 constraints for each directions)
    %  8 7 6 5 4 3 2 1
    %  7 6 5 4 3 2 1 8
    %  6 5 4 3 2 1 8 7
    %  5 4 3 2 1 8 7 6
    %  5 4 3 2 1 8 7 6
    %  6 5 4 3 2 1 8 7
    %  7 6 5 4 3 2 1 8
    %  1 7 6 5 4 3 2 1
    %  <    
    a8 + b7 + c6 + d5 + e5 + f6 + g7 + h8 = Sum  /\ 
    a7 + b6 + c5 + d4 + e4 + f5 + g6 + h7 = Sum  /\ 
    a6 + b5 + c4 + d3 + e3 + f4 + g5 + h6 = Sum  /\ 
    a5 + b4 + c3 + d2 + e2 + f3 + g4 + h5 = Sum  /\ 
    a4 + b3 + c2 + d1 + e1 + f2 + g3 + h4 = Sum  /\ 
    a3 + b2 + c1 + d8 + e8 + f1 + g2 + h3 = Sum  /\ 
    a2 + b1 + c8 + d7 + e7 + f8 + g1 + h2 = Sum  /\ 
    a1 + b8 + c7 + d6 + e6 + f7 + g8 + h1 = Sum  /\ 

    % The four entries in every 2x2 subsquare sum to 130.
    % 8x8 = 64 constraints

    a1 + a2 + b1 + b2 = Sum2  /\  
    a2 + a3 + b2 + b3 = Sum2  /\  
    a3 + a4 + b3 + b4 = Sum2  /\  
    a4 + a5 + b4 + b5 = Sum2  /\  
    a5 + a6 + b5 + b6 = Sum2  /\  
    a6 + a7 + b6 + b7 = Sum2  /\  
    a7 + a8 + b7 + b8 = Sum2  /\  
    a8 + a1 + b8 + b1 = Sum2  /\  % wrap

    b1 + b2 + c1 + c2 = Sum2  /\  
    b2 + b3 + c2 + c3 = Sum2  /\  
    b3 + b4 + c3 + c4 = Sum2  /\  
    b4 + b5 + c4 + c5 = Sum2  /\  
    b5 + b6 + c5 + c6 = Sum2  /\  
    b6 + b7 + c6 + c7 = Sum2  /\  
    b7 + b8 + c7 + c8 = Sum2  /\  
    b8 + b1 + c8 + c1 = Sum2  /\  % wrap

    c1 + c2 + d1 + d2 = Sum2  /\  
    c2 + c3 + d2 + d3 = Sum2  /\  
    c3 + c4 + d3 + d4 = Sum2  /\  
    c4 + c5 + d4 + d5 = Sum2  /\  
    c5 + c6 + d5 + d6 = Sum2  /\  
    c6 + c7 + d6 + d7 = Sum2  /\  
    c7 + c8 + d7 + d8 = Sum2  /\  
    c8 + c1 + d8 + d1 = Sum2  /\  % wrap

    d1 + d2 + e1 + e2 = Sum2  /\  
    d2 + d3 + e2 + e3 = Sum2  /\  
    d3 + d4 + e3 + e4 = Sum2  /\  
    d4 + d5 + e4 + e5 = Sum2  /\  
    d5 + d6 + e5 + e6 = Sum2  /\  
    d6 + d7 + e6 + e7 = Sum2  /\  
    d7 + d8 + e7 + e8 = Sum2  /\  
    d8 + d1 + e8 + e1 = Sum2  /\  % wrap

    e1 + e2 + f1 + f2 = Sum2  /\  
    e2 + e3 + f2 + f3 = Sum2  /\  
    e3 + e4 + f3 + f4 = Sum2  /\  
    e4 + e5 + f4 + f5 = Sum2  /\  
    e5 + e6 + f5 + f6 = Sum2  /\  
    e6 + e7 + f6 + f7 = Sum2  /\  
    e7 + e8 + f7 + f8 = Sum2  /\  
    e8 + e1 + f8 + f1 = Sum2  /\  % wrap

    f1 + f2 + g1 + g2 = Sum2  /\  
    f2 + f3 + g2 + g3 = Sum2  /\  
    f3 + f4 + g3 + g4 = Sum2  /\  
    f4 + f5 + g4 + g5 = Sum2  /\  
    f5 + f6 + g5 + g6 = Sum2  /\  
    f6 + f7 + g6 + g7 = Sum2  /\  
    f7 + f8 + g7 + g8 = Sum2  /\  
    f8 + f1 + g8 + g1 = Sum2  /\  % wrap

    g1 + g2 + h1 + h2 = Sum2  /\  
    g2 + g3 + h2 + h3 = Sum2  /\  
    g3 + g4 + h3 + h4 = Sum2  /\  
    g4 + g5 + h4 + h5 = Sum2  /\  
    g5 + g6 + h5 + h6 = Sum2  /\  
    g6 + g7 + h6 + h7 = Sum2  /\  
    g7 + g8 + h7 + h8 = Sum2  /\  
    g8 + g1 + h8 + h1 = Sum2  /\  % wrap

    h1 + h2 + a1 + a2 = Sum2  /\  
    h2 + h3 + a2 + a3 = Sum2  /\  
    h3 + h4 + a3 + a4 = Sum2  /\  
    h4 + h5 + a4 + a5 = Sum2  /\  
    h5 + h6 + a5 + a6 = Sum2  /\  
    h6 + h7 + a6 + a7 = Sum2  /\  
    h7 + h8 + a7 + a8 = Sum2  /\  
    h8 + h1 + a8 + a1 = Sum2  /\  % wrap

    % Bent Diagonals (8 constraints for each directions)
    %  1 8 7 6 6 7 8 1
    %  2 1 8 7 7 8 1 2
    %  3 2 1 8 8 1 2 3
    %  4 3 2 1 1 2 3 4
    %  5 4 3 2 2 3 4 5
    %  6 5 4 3 3 4 5 6
    %  7 6 5 4 4 5 6 7
    %  8 7 6 5 5 6 7 8
    %  \/
    a1 + b2 + c3 + d4 + d5 + c6 + b7 + a8 = Sum  /\ 
    b1 + c2 + d3 + e4 + e5 + d6 + c7 + b8 = Sum  /\ 
    c1 + d2 + e3 + f4 + f5 + e6 + d7 + c8 = Sum  /\ 
    d1 + e2 + f3 + g4 + g5 + f6 + e7 + d8 = Sum  /\ 
    e1 + f2 + g3 + h4 + h5 + g6 + f7 + e8 = Sum  /\ 
    f1 + g2 + h3 + a4 + a5 + h6 + g7 + f8 = Sum  /\ 
    g1 + h2 + a3 + b4 + b5 + a6 + h7 + g8 = Sum  /\  
    h1 + a2 + b3 + c4 + c5 + b6 + a7 + h8 = Sum  /\ 

    % Bent Diagonals (8 constraints for each directions)
    %  8 7 6 5 5 6 7 8
    %  7 6 5 4 4 5 6 7
    %  6 5 4 3 3 4 5 6
    %  5 4 3 2 2 3 4 5
    %  4 3 2 1 1 2 3 4
    %  3 2 1 8 8 1 2 3
    %  2 1 8 7 7 8 1 2
    %  1 8 7 6 6 7 8 1
    %   /\
    h1 + g2 + f3 + e4 + e5 + f6 + g7 + h8 = Sum  /\ 
    g1 + f2 + e3 + d4 + d5 + e6 + f7 + g8 = Sum  /\ 
    f1 + e2 + d3 + c4 + c5 + d6 + e7 + f8 = Sum  /\ 
    e1 + d2 + c3 + b4 + b5 + c6 + d7 + e8 = Sum  /\ 
    d1 + c2 + b3 + a4 + a5 + b6 + c7 + d8 = Sum  /\ 
    c1 + b2 + a3 + h4 + h5 + a6 + b7 + c8 = Sum  /\ 
    b1 + a2 + h3 + g4 + g5 + h6 + a7 + b8 = Sum  /\ 
    a1 + h2 + g3 + f4 + f5 + g6 + h7 + a8 = Sum  /\ 

    % Bent Diagonals (8 constraints for each directions)
    %  1 2 3 4 5 6 7 8
    %  8 1 2 3 4 5 6 7
    %  7 8 1 2 3 4 5 6
    %  6 7 8 1 2 3 4 5
    %  6 7 8 1 2 3 4 5
    %  7 8 1 2 3 4 5 6
    %  8 1 2 3 4 5 6 7
    %  1 2 3 4 5 6 7 8
    %  >
    a1 + b2 + c3 + d4 + e4 + f3 + g2 + h1 = Sum  /\ 
    a2 + b3 + c4 + d5 + e5 + f4 + g3 + h2 = Sum  /\ 
    a3 + b4 + c5 + d6 + e6 + f5 + g4 + h3 = Sum  /\ 
    a4 + b5 + c6 + d7 + e7 + f6 + g5 + h4 = Sum  /\    
    a5 + b6 + c7 + d8 + e8 + f7 + g6 + h5 = Sum  /\    
    a6 + b7 + c8 + d1 + e1 + f8 + g7 + h6 = Sum  /\    
    a7 + b8 + c1 + d2 + e2 + f1 + g8 + h7 = Sum  /\    
    a8 + b1 + c2 + d3 + e3 + f2 + g1 + h8 = Sum   

;


%output 
%[
% if j = 1 then "\n" else " " endif ++
%   show(a[i,j])
% | i, j in 1..n
%];

% show just the first row ...
output 
[
  show(a[1,j]) ++ " "
  | j in 1..n
] ++ ["\n"];


