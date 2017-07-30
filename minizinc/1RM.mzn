% 
% 1RM calculations / Comparing Reps+Weights in MiniZinc.
% 
% Calculation of 1RM for weight training and age adjustment of weights.
% See below for sources.
%
% Example: 
% We want to know how much a session of 6 reps of 87kg corresponds to
% a session of 12 reps. Answer: 
%
%   OneRM  : 101.03kg age adjusted: 152.96kg
%   Weight1: 87.00kg age adjusted: 131.72kg
%   Weight2: 70.16kg age adjusted: 106.22kg
%
% I.e. a session of 6 x 87kg corresponds to a session with 12 x 70.16kg,
% with an 1RM of 101.03kg: this is the value that binds the two sessions
% (weights x reps) to each other.
%
% Also we see the age adjusted weights for Weight1, Weight2, and 1RM.
% For a 60 yo man this corresponds to weight x 1.514.
%
% We can also study - since this is a CP model - how much one have to
% lift in order to have a 1RM of 100kg for 6 reps and 12 reps, respectively:
%
%   OneRM  : 100.00kg age adjusted: 151.40kg
%   Weight1: 86.11kg age adjusted: 130.37kg
%   Weight2: 69.44kg age adjusted: 105.14kg
%
%  [I've have been there for both 6 and 12 reps.
%    My best so far:
%    - 12 x 72kg -> 1RM of 103.68kg (age adjusted:  156.97kg)
%    -  4 x 92kg -> 1RM of 101.03kg (age adjusted 152.96kg)
%    -  6 x 87kg -> 1RM of 101.03kg (== 4 x 92kg)
%  ]
%
% Note: This don't give the same results as http://strengthlevel.com/ :
% - Weight*Reps don't give the same OneRM, this model has higher values.
%   Though brzycki's seems to be nearer than epley's.
% - Age adjustments are higher in this model.
% 
% Solver note:
% The solver must support decision variables of type var float, such as
% - Gecode
% - JaCoP
% - ECLiPSe/ic
% - Perhaps Choco (I have not been able to test it)
% (- G12/mip supports var floats but not for nonlinear constraints.)
% Both Gecode and JaCoP generates a lot of solutions.




% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

array[int] of float: AgeCoeffs; % age adjusted coefficients

int: age;

var 1.0..200.0: weight1;
var 1.0..200.0: weight2;

var 1..30: reps1;
var 1..30: reps2;

var 1.0..220.0: oneRM;

var 1.0..220.0: weight1Age;
var 1.0..220.0: weight2Age;
var 1.0..220.0: oneRMAge;

% solve satisfy;
% Gecode works better with this (though it yield a lot of solutions instead of just one)
solve :: float_search([weight1,weight2,reps1,reps2,oneRM,weight1Age,weight2Age,oneRMAge], 0.00000001, input_order, indomain_split, complete) satisfy;

constraint

  % weight1 = 87 /\ 
  reps1 = 6 /\

  % weight1 = 92 /\ 
  % reps1 = 4 /\

  
  reps2   = 12 /\
  weight2 = 72 /\ 

  % oneRM = 100 /\

  % oneRMAge = 100 /\

  % epley's formula for 1RM given weight and reps
  % oneRM = weight1*(1 + reps1/30) /\
  % oneRM = weight2*(1 + reps2/30)

  % brzycki's formula: slightly lower than epley's
  oneRM = weight1 * (36/(37 - reps1)) /\
  oneRM = weight2 * (36/(37 - reps2)) 

  /\
  weight1Age = weight1*AgeCoeffs[age-30+1] /\
  weight2Age = weight2*AgeCoeffs[age-30+1] /\
  oneRMAge = oneRM*AgeCoeffs[age-30+1] 

  /\ trace("agecoeff: \(AgeCoeffs[age-30+1])\n")

;

output [

  "OneRM   ( " ++ show_int(2,1) ++ " rep): "     ++ show_float(2,2,oneRM)   ++ "kg age adjusted: " ++ show_float(3,2,oneRMAge) ++ "kg\n",
  "Weight1 (" ++ show_int(2,reps1) ++ " reps): " ++ show_float(2,2,weight1) ++ "kg age adjusted: " ++ show_float(3,2,weight1Age) ++ "kg\n",
  "Weight2 (" ++ show_int(2,reps2) ++ " reps): " ++ show_float(2,2,weight2) ++ "kg age adjusted: " ++ show_float(3,2,weight2Age) ++ "kg\n",
];

age = 60;


% Starts at 30
AgeCoeffs = 
[
   1    , 1.016, 1.031, 1.046, 1.059, 
   1.072, 1.083, 1.096, 1.109, 1.122, 
   1.135, 1.149, 1.162, 1.176, 1.189, 
   1.203, 1.218, 1.233, 1.248, 1.263, 
   1.279, 1.297, 1.316, 1.338, 1.361, 
   1.385, 1.411, 1.437, 1.462, 1.488, 
   1.514, 1.541, 1.568, 1.598, 1.629, 
   1.663, 1.699, 1.738, 1.779, 1.823, 
   1.867, 1.910, 1.953, 2.004, 2.060, 
   2.117, 2.181, 2.255, 2.336, 2.419, 
   2.540, 2.597, 2.702, 2.831, 2.981, 
   3.153, 3.352, 3.580, 3.843, 4.145, 
   4.493 
];


/*

Calculations of 1 RM based on nRM;
The formulas is from Maud & Foster  "Physiological Assessments of"
" Human Fitneess" (2 nd ed), page 140.

(I assume kg in the formulas, but not sure if that' s correct or if it would matter.)
     
Also see: https://en.wikipedia.org/wiki/One - repetition_maximum ;
 
The age coefficient (1.514 for age 60) is e.g from Meltzer - Faber Age coeffcients:
   http://www.mastersweightlifting.org/forms/meltzer.htm

also:
   http://dinosaurtraining.blogspot.se/2016/12/the-difference-between-age-30-and-age-60.html

Age coefficients: http://www.mastersweightlifting.org/forms/meltzer.htm 

coeffs = {
   {30, 1}, {31, 1.016}, {32, 1.031}, {33, 1.046}, {34, 1.059}, 
   {35, 1.072}, {36, 1.083}, {37, 1.096}, {38, 1.109}, {39, 1.122}, 
   {40, 1.135}, {41, 1.149}, {42, 1.162}, {43, 1.176}, {44, 1.189}, 
   {45, 1.203}, {46, 1.218}, {47, 1.233}, {48, 1.248}, {49, 1.263}, 
   {50, 1.279}, {51, 1.297}, {52, 1.316}, {53, 1.338}, {54, 1.361}, 
   {55, 1.385}, {56, 1.411}, {57, 1.437}, {58, 1.462}, {59, 1.488}, 
   {60, 1.514}, {61, 1.541}, {62, 1.568}, {63, 1.598}, {64, 1.629}, 
   {65, 1.663}, {66, 1.699}, {67, 1.738}, {68, 1.779}, {69, 1.823}, 
   {70, 1.867}, {71, 1.91}, {72, 1.953}, {73, 2.004}, {74, 2.06}, 
   {75, 2.117}, {76, 2.181}, {77, 2.255}, {78, 2.336}, {79, 2.419}, 
   {80, 2.540}, {81, 2.597}, {82, 2.702}, {83, 2.831}, {84, 2.981}, 
   {85, 3.153}, {86, 3.352}, {87, 3.58}, {88, 3.843}, {89, 4.145}, 
   {90, 4.493}};


Strangely brzycki's formula yield a lower 1Rm than 10RM!
Changed to Wikipedia's version:
brzycki[weight_, reps_] :=  
  weight * (36./(37 - reps));(* weight/1.0278 - 0.0278*reps*)

epley[weight_, reps_] := 0.033*weight *reps + weight;
epley2[weight_, reps_] := weight (1 + reps/30.);
oconner[weight_, reps_] := weight*(1 + 0.025*reps);
lombardi[weight_, reps_] := weight * reps^0.10;
mcglothin[weight_, reps_] := 100*weight/(101.3 - 2.67123*reps);
wathan[weight_, reps_] := 100*weight/(48.8 + 53.8 Exp[-0.075 reps]); 

*/
