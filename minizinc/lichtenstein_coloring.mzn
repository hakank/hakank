% 
% Lichtenstein coloring problem in MiniZinc.
%
% From 
% http://bit-player.org/2008/the-chromatic-number-of-liechtenstein
% and
% """
% It seems that Liechtenstein is divided into 11 communes, which 
% emphatically do not satisfy the connectivity requirement of the four 
% color map theorem. Just four of the communes consist of a single 
% connected area (Ruggell, Schellenberg and Mauren in the north, and 
% Triesen in the south). 
% ...
% In the map above, each commune is assigned its own color, and so we 
% have an 11-coloring. It’s easy to see we could make do with fewer 
% colors, but how many fewer? I have found a five-clique within the map; 
% that is, there are five communes that all share a segment of border 
% with one another. It follows that a four-coloring is impossible. Is 
% there a five-coloring? What is the chromatic number of Liechtenstein?
% """
%
% Also see
% http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

%
% communes
%
int: BalzersC = 1;
int: EschenC = 2;
int: GamprinC = 3;
int: MaurenC = 4;
int: PlankenC = 5;
int: RuggellC = 6;
int: SchaanC = 7;
int: SchellenbergC = 8;
int: TriesenC = 9;
int: TriesenbergC = 10;
int: VaduzC = 11;

%
% enclaves/exclaves
%
int: Balzers1 = 1;
int: Balzers2 = 2;
int: Balzers3 = 3;
int: Eschen1 = 4;
int: Eschen2 = 5;
int: Eschen3 = 6;
int: Gamprin1 = 7;
int: Gamprin2 = 8;
int: Mauren = 9;
int: Planken1 = 10;
int: Planken2 = 11;
int: Planken3 = 12;
int: Planken4 = 13;
int: Ruggell = 14;
int: Schaan1 = 15;
int: Schaan2 = 16;
int: Schaan3 = 17;
int: Schaan4 = 18;
int: Schaan5 = 19;
int: Schellenberg = 20;
int: Triesen = 21;
int: Triesenberg1 = 22;
int: Triesenberg2 = 23;
int: Vaduz1 = 24;
int: Vaduz2 = 25;
int: Vaduz3 = 26;
int: Vaduz4 = 27;
int: Vaduz5 = 28;
int: Vaduz6 = 29;


int: num_communes = 11;
int: num_colors = 11; % num_communes; % 6
int: num_edges = 45;
int: num_enclaves = 29;

%
% the enclaves and corresponding commune
%
array[1..num_enclaves] of 1..num_communes: cc = 
[
BalzersC, BalzersC, BalzersC, 
EschenC, EschenC, EschenC, 
GamprinC, GamprinC, 
MaurenC, 
PlankenC, PlankenC, PlankenC, PlankenC, 
RuggellC, 
SchaanC, SchaanC, SchaanC, SchaanC, SchaanC, 
SchellenbergC, 
TriesenC, 
TriesenbergC, TriesenbergC, 
VaduzC, VaduzC, VaduzC, VaduzC, VaduzC, VaduzC
];

% neighbours
array[1..num_edges, 1..2] of 1..num_enclaves: Lichtenstein;

% colors for the en-/exclaves
array[1..num_enclaves] of var 1..num_communes: color;

% colors for the communes
array[1..num_communes] of var 1..num_colors: color_communes;

% what colors are used (for minimizing number of colors)
array[1..num_colors] of var 0..1: color_used;

% number of colors used
var 1..num_communes: n_colors = sum(color_used);


% solve minimize n_colors;
% if num_colors is set to 5 or 6, test with "largest" instead of "first_fail"
solve :: int_search(
        color ++ 
        % color_used ++
        color_communes ++
        [n_colors]
        , 
        first_fail, indomain_min, complete) 
        % largest, indomain_min, complete) 
        minimize n_colors;
        % satisfy;

constraint

   % 
   forall(i in 1..num_colors) (
      color_used[i] = 1 <-> exists(c in 1..num_enclaves) (color[c] = i)
   )
   /\
   % all neighbours must have different colors
   forall(i in 1..num_edges) (
        color[Lichtenstein[i,1]] != color[Lichtenstein[i,2]]
   )
      
   /\ % first commune (Balzers) has color 1
   color_communes[1] = 1

   /\ % exclaves of the same commune must have the same color
   forall(i, j in 1..num_enclaves where i != j) (
      cc[i] = cc[j] -> color[i] = color[j]
   )

   /\
   % connection between commune and en-/exclaves
   forall(c in 1..num_communes) (
      exists(e in 1..num_enclaves) (
         color_communes[c] = color[e]
         /\
         cc[e] = c
      )
   )

   % /\ n_colors <= 5 % for solve satisfy

;


% This map is from
% http://blog.mikael.johanssons.org/archive/2008/10/on-the-chromatic-number-of-lichtenstein/
Lichtenstein = array2d(1..num_edges, 1..2, [
  Ruggell, Schellenberg,
  Ruggell, Gamprin1,
  Schellenberg, Mauren,
  Schellenberg, Eschen1,
  Mauren, Eschen1,
  Gamprin1, Eschen2,
  Gamprin1, Vaduz2,
  Gamprin1, Schaan1,
  Gamprin1, Planken3,
  Gamprin1, Eschen1,
  Eschen1, Gamprin2,
  Eschen1, Planken1,
  Eschen2, Schaan1,
  Vaduz3, Schaan1,
  Vaduz2, Schaan1,
  Planken3, Schaan1,
  Planken2, Schaan1,
  Schaan1, Planken1,
  Schaan1, Planken4,
  Schaan1, Vaduz1,
  Gamprin2, Eschen3,
  Eschen3, Vaduz4,
  Eschen3, Schaan2,
  Vaduz4, Schaan2,
  Vaduz4, Planken1,
  Schaan2, Planken1,
  Planken1, Schaan3,
  Vaduz1, Triesenberg1,
  Vaduz1, Triesen,
  Planken4, Triesenberg1,
  Planken4, Balzers2,
  Balzers2, Vaduz5,
  Balzers2, Schaan4,
  Vaduz5, Schaan4,
  Schaan4, Triesenberg1,
  Schaan4, Vaduz6,
  Schaan4, Triesenberg2,
  Triesenberg1, Vaduz6,
  Triesenberg1, Triesen,
  Triesenberg1, Balzers3,
  Triesen, Balzers3,
  Triesen, Balzers1,
  Triesen, Schaan5,
  Vaduz6, Schaan5,
  Triesenberg2, Schaan5
]);


output [
  "n_colors: ", show(n_colors), "\n",
  "color_communes: ", show(color_communes), "\n",
  "color: ", show(color), "\n",

]
;