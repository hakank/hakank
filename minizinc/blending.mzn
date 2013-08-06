% 
% Blending problem in MiniZinc.
%
% From the OPL model blending.mod
% 
% Compare with the Comet model:
% http://www.hakank.org/comet/blending.co
% 
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

array[Metals] of 0.0..100.0: CostMetal = [22.0, 10.0, 13.0];
array[Raws]   of 0.0..100.0: CostRaw   = [6.0, 5.0];
array[Scraps] of 0.0..100.0: CostScrap = [7.0, 8.0];
array[Ingos]  of 0.0..100.0: CostIngo  = [9.0];
array[Metals] of 0.0..100.0: Low       = [0.05, 0.30, 0.60];
array[Metals] of 0.0..100.0: Up        = [0.10, 0.40, 0.80];

array[Metals, Raws] of 0.0..1.0: PercRaw = array2d(Metals, Raws, [0.20, 0.01, 0.05, 0.0, 0.05, 0.30]); 
array[Metals, Scraps] of 0.0..1.0: PercScrap =  array2d(Metals, Scraps, [0.0, 0.01, 0.60, 0.0, 0.40, 0.70]);
array[Metals, Ingos] of 0.0..1.0: PercIngo =  array2d(Metals, Ingos, [0.10, 0.45, 0.45]);
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

solve minimize z;

constraint
   z >= 0.0
   /\
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