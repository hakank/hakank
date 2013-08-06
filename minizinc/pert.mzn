%
% Simple PERT model in Minizinc
%
% From Pascal van Hentenryck 
% "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
% http://citeseer.ist.psu.edu/300151.html
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: maxTime;
int: n;
int: numDependencies;
array[1..n] of 0..maxTime: Times; % time per activity
array[1..numDependencies, 1..2] of 1..n: Dependencies; 
array[1..n] of var 0..maxTime: Start; % when the activity start

var int: SumTimes = sum(i in 1..n) (Start[i]);

% solve satisfy;
% solve minimize SumTimes; % sum(i in 1..n) (Start[i]);
solve minimize Start[n]; % minimize Send

%
% data
%
maxTime = 30;
n = 11;
     %   a  b  c  d  e  f  g  h  j  k  Send 
Times = [7, 3, 1, 8, 1, 1, 1, 3, 2, 1, 1];
numDependencies = 15;

% Dependencies
% Note: There is no Si
Dependencies = 
array2d(1..numDependencies, 1..2,
[
  2,1,  % Sb >= Sa + 7
  4,1,  % Sd >= Sa + 7
  3,2,  % Sc >= Sb + 3
  5,3,  % Se >= Sc + 1
  5,4,  % Se >= Sd + 8
  7,3,  % Sg >= Sc + 1
  7,4,  % Sg >= Sd + 8
  6,4,  % Sf >= Sd + 8
  6,3,  % Sf >= Sc + 1
  8,6,  % Sh >= Sf + 1
  9,8,  % Sj >= Sh + 3
  10,7, % Sk >= Sg + 1
  10,5, % Sk >= Se + 1
  10,9, % Sk >= Sj + 2
  11,10 % Send >= Sk + 1
]);

constraint
   forall(i in 1..numDependencies) (
      Start[Dependencies[i,1]] >= (Start[Dependencies[i,2]] + Times[Dependencies[i,2]])
   )
   % for solve satisfy
   % /\ SumTimes <= 147
   % /\ Start[n] <= 22
;

output
[
  "Start: " ++ show(Start) ++ "\n" ++
  "SumTimes: " ++ show(SumTimes) ++ "\n" ++
  "Start[n]: " ++ show(Start[n]) ++ "\n" 

];