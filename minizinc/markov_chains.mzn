% 
% Markov Chains in MiniZinc.
% 
% From the (swedish) book "Statistisk Dataanalys", page 299ff.
% 
% Transition matrix of market shares for three products A, B, and C.
%   
%   From   To
%          A    B     C
%     A   0.7  0.1   0,2
%     B   0.2  0.6   0.2
%     C   0.4  0.1   0.5
%
% What is the stable state of the transitions?
%
% Note: This model also test the reverse problem of generating a transition
% matrix given the stable state.


% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


% Result for minizinc version 0.7.1:
% eplex, ic, mip, and gecode all finds:
% x = [ 0.5142857142857142, 0.2, 0.2857142857142858 ]
% gecode: [0.514285714285714, 0.2, 0.285714285714286]

%
% minizinc and eplex are fast for larger matrices. ic is not.

% For the "talkative" version
% var 0.0..1.0: A;
% var 0.0..1.0: B;
% var 0.0..1.0: C;

int: n;
array[1..n] of var 0.0..1.0: x;             % the decision variables
array[1..n, 1..n] of var 0.0..1.0: transitions; % the transition matrix

% solve :: float_search([transitions[i,j] | i,j in 1..n], 0.001, "input_order", "indomain_split", "complete") satisfy;
solve satisfy;
% for the reverse mode:
% solve maximize sum(j in 1..n) (transitions[1,j]);

constraint
  % the "talkative" version
  % 0.7*A + 0.2*B + 0.4*C = A
  % /\
  % 0.1*A + 0.6*B + 0.1*C = B
  % /\
  % 0.2*A + 0.2*B + 0.5*C = C
  % /\ A+B+C = 1.0

  % general solution
  forall(i in 1..n) (
    x[i] = sum(j in 1..n) (transitions[i,j]*x[j])
  )
  /\
  sum(i in 1..n) (x[i]) = 1.0 

  % /\ % For the reversed problem, i.e. generate the transition matrix given
     %% the stable state.
     %% As of writing (2008-05-21) only ic can handle this reversed mode.
     %% for n = 3
     % x = [ 0.5142857142857142, 0.2, 0.2857142857142858 ]
     %% for n = 5
     % x = [0.3,0.15,0.1,0.15,0.3]
     %% for n = 10
     % x = [0.6,0.2,0.1, 0.0,0.0,0.0,0.0,0.0,0.0, 0.1]

;

output [
   show(x), "\n"
] 
++
[
  if j = 1 then "\n" else " " endif ++
  show(transitions[i,j])
  | i,j in 1..n
]

;

%
% data
%
% n = 5; % for the reverse mode test
% n = 10;

n = 3;
% transition matrix (older minizinc/fz/eclipse)
% note: the matrix is transposed compared to the description above
% which means that the _columns_ must sum to 1

transitions = 
array2d(1..n, 1..n,
 [| 0.7,0.2,0.4
  | 0.1,0.6,0.1  
  | 0.2,0.2,0.5 
|]);



% transitions = array2d(1..n,1..n, [|
%  0.01782,0.54024,0.46916,0.85467,0.30838,0.62996,0.42815,0.22189,0.37009,0.74681
% |0.04633,0.04427,0.14569,0.03971,0.50937,0.09334,0.01055,0.12776,0.36217,0.04528
% |0.12289,0.22650,0.12335,0.02186,0.10853,0.14409,0.05414,0.01577,0.01709,0.03761
% |0.28441,0.01805,0.09189,0.04428,0.00565,0.00538,0.16482,0.20185,0.15423,0.11391
% |0.00980,0.07623,0.03248,0.00753,0.01028,0.00424,0.12144,0.11167,0.05904,0.00009
% |0.15721,0.09004,0.07690,0.00448,0.03511,0.05270,0.00859,0.00057,0.00754,0.04810
% |0.29881,0.00202,0.00490,0.00909,0.00343,0.00296,0.17159,0.05433,0.00924,0.00041
% |0.02110,0.00004,0.02863,0.00255,0.01232,0.06338,0.02043,0.16868,0.00080,0.00129
% |0.01843,0.00002,0.02181,0.00453,0.00290,0.00011,0.00819,0.00118,0.00312,0.00163
% |0.02320,0.00259,0.00519,0.01130,0.00403,0.00384,0.01210,0.09630,0.01668,0.00487
% |]);

