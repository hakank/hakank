% 
% Barbell/dumbbell weights in MiniZinc.
% 
% Calculate how to combine weight plates for barbell / dumbells.
% The variable "total" is the total weight including the barbell/dumbbell
% The result is the weights per side.
%
% * For the barbell, we require 2 weight plates of each weight (one of each side 
%   of the barbell.
% * For the dumbbells, we require 4 weight plates of each weight: one of each side
%   and for two dumbbells.
%   Note: For exercises that requires just one dumbbell (e.g. dumbbell row) then
%   one can either ignore this or change req_weights to 2 and change the 
%   weight_avail to the same as barbell. See weights_dumbbell_one.fzn
%
% Data is in the data files for my (current) home gym:
%   - Barbell
%     http://hakank.org/minizinc/weights_barbell.dzn
%   - Two dumbbells (the usual case) 
%     http://hakank.org/minizinc/weights_dumbbell.dzn
%   - Just one dumbbell (e.g. for dumbbell row)
%     http://hakank.org/minizinc/weights_dumbbell_one.dzn
%
% You might have to change the values in order to fit your own case.
% (E.g. my barbell weighs 7kg, not the normal 20kg.)
%
% Some approaches:
%   - to look for the maximimal weight: use the objective "maximize total"
%   - to look for the minimal number of weight plates for a certain total:
%     Comment out (and change) this constraint
%          constraint total = xxx; 
%     and use the objective "minimize num_selected_weights";
%
% Also, not that this model mix var float and var int, so the solver must support
% that. Some recommendes solvers:
%   - Gecode 
%   - JaCoP
%  

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: num_weights;
array[1..num_weights] of float: weights;
array[1..num_weights] of int: weight_avail;
int: req_weights;
float: bell_weight;
string: bell_type;


% decision variables
array[1..num_weights] of var 0..9: x;

var float: total = bell_weight + sum([x[i]*2*weights[i] | i in 1..num_weights]);
var int: num_selected_weights = sum([x[i] | i in 1..num_weights]);

% solve satisfy;
solve minimize num_selected_weights; % for minimizing the number of selected plates
% solve maximize total; % what't the largest total weight
% solve :: int_search(x, input_order, indomain_min, complete) satisfy;

constraint
  % ensure that we have enough couple of weight plates
  forall(w in 1..num_weights) (
     x[w] <= weight_avail[w]
  )
;

% Calculate the total weight (including weight of barbell/dumbbell)
constraint total = 24;

output [
  "\ntotal: \(total)kg\nnum_selected_weights: \(num_selected_weights)\n" ++
  "x: \(x)\n\n" ++
  "Weight of " ++ bell_type ++ ": \(bell_weight)kg\n" ++
  "Weights per side (reversed order):\n"
]
++
[
  if fix(x[num_weights-w+1]) > 0 then
    "\(x[num_weights-w+1]) of \(weights[num_weights-w+1])kg\n"
  else 
    ""
  endif
  | w in 1..num_weights
]
;

%
% Data.
% See the datafiles mentioned above...
%

%%
%% Barbell weights.
%%
%% Note: In order to work, one must have a _pair_ of weight plates, i.e. one of 
%% each side of the barbell. If you have four 2kg plates, then it's 
%%  represented as a 2 in weight_avail
%%
% bell_type = "barbell";
% num_weights = 10; % total number of weights
% req_weights = 2; % 2 weight plates in total (one of each side)
%% the weights (in kg)
% weights =      [0.50,1.0,1.25,2.0,2.5,5.0,10.0,20.0,25.0,50.0];
% % number of weight plates available
% % E.g. I have 4 plates of 0.5kg -> 2 in weight_avail, and 2 weights of 10kg -> 1.
% weight_avail = [   2,  2,  2,   2,  1,  2,   1,   0,   0,   0];  
% bell_weight = 7.0;



%%
%% Dumbbell weights.
%%
%% Note: In order to work, one must have four weight plates for a certain
%% weight, i.e. one plate of each side of the dumbbell, and we have two dumbbells.
%% If you have four 2kg plates, then it's represented as a 1 in weight_avail.
%%
% bell_type = "dumbbell";
% num_weights = 10; % total number of weights
% req_weights = 4; % 4 weights in total (two dumbbells and two on each barbell)
% % the weights (in kg)
% weights =      [0.50,1.0,1.25,2.0,2.5,5.0,10.0,20.0,25.0,50.0];
%% number of weight plates available. Divide the total number of weight plates by 4 
%% Eg. I have 12 0.5kg weights -> 3 in weight_avail, and 4 1.0kg weights (-> 1), etc.
% weight_avail = [   3,  1,  1,   1,  0,  1,   0,   0,   0,   0];  
% bell_weight = 1.5;
