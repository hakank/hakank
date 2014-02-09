% 
% Blending problem in MiniZinc.
%
% From the OPL model blending.mod
% 
% Compare with the Comet model:
% http://www.hakank.org/comet/blending.co
% 
% Solution from G12/mip:
%
%   p: [0.04666666666666576, 0.0, 0.0]
%   r: [0.0, 0.0]
%   s: [17.41666666666666, 30.33333333333334]
%   i: [32]
%   metal: [3.5500000000000003, 24.849999999999994, 42.6]
%   z: 653.61



% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


int: NbMetals = 3;
int: NbRaw = 2;
int: NbScrap = 2;
int: NbIngo = 1;
set of int: Metals = 1..NbMetals;
set of int: Raws = 1..NbRaw;
set of int: Scraps = 1..NbScrap;
set of int: Ingos = 1..NbIngo;

array[Metals] of float: CostMetal = [22.0, 10.0, 13.0];
array[Raws]   of float: CostRaw   = [6.0, 5.0];
array[Scraps] of float: CostScrap = [7.0, 8.0];
array[Ingos]  of float: CostIngo  = [9.0];
array[Metals] of float: Low       = [0.05, 0.30, 0.60];
array[Metals] of float: Up        = [0.10, 0.40, 0.80];

array[Metals, Raws] of float: PercRaw = array2d(Metals, Raws, [0.20, 0.01, 0.05, 0.0, 0.05, 0.30]); 
array[Metals, Scraps] of float: PercScrap =  array2d(Metals, Scraps, [0.0, 0.01, 0.60, 0.0, 0.40, 0.70]);
array[Metals, Ingos] of float: PercIngo =  array2d(Metals, Ingos, [0.10, 0.45, 0.45]);
float: Alloy  = 71.0;


array[Metals] of var 0.0..100.0: p;
array[Raws] of var 0.0..100.0: r;
array[Scraps] of var 0.0..100.0: s;
array[Ingos] of var 0..100: i;
array[Metals] of var 0.0..100.0: metal; % [j in Metals](m, Low[j] * Alloy, Up[j] * Alloy);

var 0.0..1000.0: z = 
        sum(j in Metals) (CostMetal[j] * p[j]) + 
        sum(j in Raws)   (CostRaw[j]   * r[j]) + 
        sum(j in Scraps) (CostScrap[j] * s[j]) +  
        sum(j in Ingos)  (CostIngo[j]  * int2float(i[j]))
;

% solve minimize z;
% solve :: float_search(p++r++s++metal,0.0001,input_order, indomain_split, complete) minimize z;
solve :: seq_search(
   [
     float_search(metal++p++s++r,0.00001,input_order, indomain_split, complete),
     int_search(i,first_fail, indomain_median, complete),
   ]) minimize z;

constraint
   % z >= 0.0 /\
  forall( j in Metals ) (
    metal[j] >= Low[j] * Alloy /\
    metal[j] <= Up[j] * Alloy /\
  
    metal[j] = p[j] + 
             sum(k in Raws)   (PercRaw[j,k]   * r[k]) +
             sum(k in Scraps) (PercScrap[j,k] * s[k]) +
             sum(k in Ingos)  (PercIngo[j,k]  * int2float(i[k]))
  )
  /\
  sum( j in Metals ) (metal[j]) = Alloy
;


output
[
  "p: " ++ show(p) ++ "\n" ++ 
  "r: " ++ show(r) ++ "\n" ++ 
  "s: " ++ show(s) ++ "\n" ++ 
  "i: " ++ show(i) ++ "\n" ++ 
  "metal: " ++ show(metal) ++ "\n" ++ 
  "z: " ++ show(z) ++ "\n" 
];