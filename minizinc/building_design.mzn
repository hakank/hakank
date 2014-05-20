% 
% Building design in MiniZinc.
% 
% From Practial Optimization with Mathematica (PDF-fil), sid 16
%
% """"
% To save energy costs for heating and cooling, an architect is considering de- 
% signing a partially buried rectangular building. The total floor space needed 
% is 20,000 m^2. 
% Lot size limits the building plan dimension to 50 m. It has already 
% been decided that the ratio between the plan dimensions must be equal to 
% the golden ratio (1.618) and that each story must be 3.5 m high. The heating 
% and cooling costs are estimated at $100 per m^2 of the exposed surface area of 
% the building. The owner has specified that the annual energy costs should not 
% exceed $225,000. Formulate the problem of determining building dimensions 
% to minimize cost of excavation. 
% """"

%% Answer at 
%%    - page 78f (graphic with 2 variables)
%%    - 175 f

%% Correct answer should be:
%%   d* = 80.03 m h* = 13.31m w* = 21.53 m f* = 60020.5 m^3 

% JaCoP 4.1 give this solution
%   z: 60021.08600023597
%   n: 27
%   [d,h,l,w]: [81.02846610032, 13.47153389968, 34.6196262042, 21.39655513239, 60021.08600023597]

% 
% Most solvers can't handle this:
% 
%   G12 MIP: symbol error: `float_div_reif' undeclared
%   ECLiPSe/ic: calling an undefined procedure fzn_ic : float_div_reif
%   ECLiPSe/eplex: calling an undefined procedure fzn_eplex : array_bool_and
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


var int: n; %  >=1 integer := 3; # number of stories (integer)
var float: d; %  >=0;# := 80; # depth of building below ground
var float: h; %  >=0;# := 13; # height of building above ground
var float: l; % >=0;# := 10; # length of building in plan
var float: w; % >=0;# := 21; # width of building in plan

var float: z = d*l*w;

% solve minimize z;
solve :: float_search([d,h,w,l], 0.001, input_order,indomain_split,complete) minimize z;

constraint
        n >= 0 /\
        d >= 0.0 /\ h >= 0.0 /\ l >= 0.0 /\ w >= 0.0
        /\
        n <= 100 /\
        d <= 100.0 /\ h <= 100.0 /\ l <= 100.0 /\ w <= 100.0

        /\
        (d+h)/int2float(n) = 3.5
        /\
        l = 1.618*w
        /\
        int2float(n)*l*w >= 20000.0
        /\
        l <= 50.0
        /\
        w <= 50.0
        /\
        100.0*(2.0*h*l+2.0*h*w + l*w) <= 225000.0
;

% Rephrase the problemet to:
% minimize f:
%        1.618*d*w^2;
%
%subject to
%        c1: 100*(5.236*h*w + 1.618*w^2) <= 225000;
%        c2: 1.618*w <= 50;
%        c3: 0.4622857*(d+h)*w^2 >= 20000;


output
[ 
  "z: ", show(z), "\n",
  "n: ", show(n), "\n",
  "[d,h,l,w]: ", show([d,h,l,w,z]), "\n"
];