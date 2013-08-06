% 
% Parallel resistors in MiniZinc.
% 
% From Marriott & Stuckey "Programming with constraints", page 134f.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% we have to limit the possible values...
set of int: r = -10..10;

%
% parallel_resistors rule page 134
%
predicate parallel_resistors(var r: V, var r: I, var r: R1, var r: R2) = 
  let {
     var r: I1,
     var r: I2
  }
  in
  V = I1*R1 
  /\ V = I2*R2
  /\ I1+I2 = I
;

solve satisfy;


%% first goal on page 134.
var r: V;
var r: I;
var r: R1;
var r: R2;
constraint
        % gp134a(V,I,R1,R2) :-
	parallel_resistors(V,I,R1,R2) 
        /\ 
        R1 = 10 
        /\ 
        R2 = 5
;


%% second goal on page 134.
% var r: I;
% var r: R;
% constraint 
%    parallel_resistors(10,I,R,R)
% ;


%% goal for example 4.2 page 135
% var r: VA;
% var r: IA;
% var r: VB;
% var r: IB;
% var r: V;
% var r: I;
% constraint 
%      parallel_resistors(VA, IA, 10, 5) 
%      /\
%      parallel_resistors(VB, IB, 8, 3) 
%      /\
%      VA + VB = V
%      /\ 
%      I = IB
%      /\ 
%      I = IA
% ;



output
[  
  show([V,I,R1,R2])
];