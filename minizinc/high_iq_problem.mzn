% 
% "High IQ Problem" in MiniZinc.
% 
%
% From Simon Colton's AI lecture "Constraint Satisfaction Problems"
% 
% http://www.doc.ic.ac.uk/~sgc/teaching/v231/lecture15.html
% 
% """ 
% I was perhaps most proud of AI on a Sunday. On this particular Sunday, 
% a friend of mine found an article in the Observer about the High-IQ society, 
% a rather brash and even more elitist version of Mensa. Their founder said 
% that their entrance test was so difficult that some of the problems had 
% never been solved. The problem given below was in the Observer as such an 
% unsolved problem. After looking at it for a few minutes, I confidently 
% told my friend that I would have the answer in half an hour. 
%
% After just over 45 minutes, I did indeed have an answer, and my friend 
% was suitably impressed. See the end of these notes for the details. Of 
% course, I didn't spend my time trying to figure it out (if you want to 
% split the atom, you don't sharpen a knife). Instead, I used the time to 
% describe the problem to a constraint solver, which is infinitely better 
% at these things than me. The constraint solver is part of good old 
% Sicstus Prolog, so specifying the problem was a matter of writing it as 
% a logic program - it's worth pointing out that I didn't specify how to 
% find the solution, just what the problem was. With AI programming 
% languages such as Prolog, every now and then the intelligence behind 
% the scenes comes in very handy. Once I had specified the problem to 
% the solver (a mere 80 lines of Prolog), it took only one hundredth 
% of a second to solve the problem. So not only can the computer solve 
% a problem which had beaten many high IQ people, it could solve 100 of 
% these "difficult" problems every second. A great success for AI.
%
% (
%   Caption of picture: The square below contains 24 smaller squares, each 
%   with a different integral size. Determine the length of the shaded square.
% )
% """


%
% Answer:
% [1, 2, 3, 4, 5, 8, 9, 14, 16, 18, 20, 29, 30, 31, 33, 35, 38, 39, 43, 51, 55, 56, 64, 81, 175]
%

% Note: This model is based on the Prolog code last on the page mentioned above.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: Top = 200; 

var 1..Top:  L1;
var 1..Top:  L2;
var 1..Top:  L3;
var 1..Top:  L4;
var 1..Top:  L5;
var 1..Top:  L6;
var 1..Top:  L7;
var 1..Top:  L8;
var 1..Top:  L9; 
var 1..Top:  L10; 
var 1..Top:  L11; 
var 1..Top:  L12; 
var 1..Top:  L13; 
var 1..Top:  L14; 
var 1..Top:  L15; 
var 1..Top:  L16; 
var 1..Top:  L17; 
var 1..Top:  L18; 
var 1..Top:  L19; 
var 1..Top:  L20; 
var 1..Top:  L21; 
var 1..Top:  L22; 
var 1..Top:  L23; 
var 1..Top:  L24; 
var 1..Top:  L25; 

array[1..25] of var 1..Top: LD = [L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,L17,L18,L19,L20,L21,L22,L23,L24,L25];

var int:  LHS;
var int:  RHS;

% solve satisfy;
solve :: int_search(LD ++ [LHS, RHS], first_fail, indomain_min, complete) satisfy;

constraint

    all_different(LD)
    /\ 

    % Ordering
    L1 < L2 /\  L2 < L3 /\  L3 < L4 /\  L4 < L5 /\  L5 < L6 /\  L6 < L7 /\  
    L7 < L8 /\  L8 < L9 /\  L9 < L10 /\  L10 < L11 /\  L11 < L12 /\  L12 < L13 /\  
    L13 < L14 /\  L14 < L15 /\  L15 < L16 /\  L16 < L17 /\  L17 < L18 /\  
    L18 < L19 /\  L19 < L20 /\  L20 < L21 /\  L21 < L22 /\  L22 < L23 /\  
    L23 < L24 /\  L24 < L25 /\ 

    % Sum of Squares Constraint
    L1*L1 + L2*L2 + L3*L3 + L4*L4 + L5*L5 + L6*L6 + L7*L7 + L8*L8 + 
    L9*L9 + L10*L10 + L11*L11 + L12*L12 + L13*L13 + L14*L14 + L15*L15 + 
    L16*L16 + L17*L17 + L18*L18 + L19*L19 + L20*L20 + L21*L21 + L22*L22 + 
    L23*L23 + L24*L24 = L25*L25 /\ 

    % Length Constraints
    L1 + L3 = L4 /\  L4 + L1 = L5 /\ 
    L4 + L5 = L7 /\  L5 + L7 = L8 /\ 
    L3 + L4 + L7 = L9 /\  L1 + L5 + L8 = L11 /\ 
    L2 + L12 = L14 /\  L2 + L14 = L15 /\ 
    L2 + L15 = L16 /\  L10 + L11 = L17 /\ 
    L7 + L8 + L9 = L18 /\  L6 + L16 = L19 /\ 
    L6 + L19 = L20 /\  L9 + L18 = L21 /\ 
    L10 + L17 = L22 /\  L14 + L15 = L23 /\ 
    L13 + L20 = L24 /\  L21 + L22 + L23 = L25 /\ 
    L18 + L21 + L24 = L25 /\  L19 + L20 + L24 = L25 /\ 
    L15 + L16 + L19 + L23 = L25 /\ 
    
    % Double check the Answer
    LHS = L1*L1 + L2*L2 + L3*L3 + L4*L4 + L5*L5 + L6*L6 + L7*L7 + 
    L8*L8 + L9*L9 + L10*L10 + L11*L11 + L12*L12 + L13*L13 + L14*L14 + 
    L15*L15 + L16*L16 + L17*L17 + L18*L18 + L19*L19 + L20*L20 + L21*L21 + 
    L22*L22 + L23*L23 + L24*L24 /\ 

    RHS = L25*L25

;

output [
  "LD: ", show(LD),"\n",
  "LHS: ", show(LHS), "\n",
  "RHS: ", show(RHS), "\n",

];