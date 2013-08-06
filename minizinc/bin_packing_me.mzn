%
% Simple Bin Packing problem in Minizinc
%
% Note the symmetry breaking constraints.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

int: num_stuff;                    % number of things to pack
array[1..num_stuff] of int: stuff; % the value/weight of the things to pack
int: bin_capacity;                 % all bins have the same capacity

% Number of bins cannot exceed num_stuff...
int: num_bins; % num_stuff;

%
% decision variables
%
% either the thing is packed or not
array[1..num_bins, 1..num_stuff] of var 0..1: bins;

% calculate how many things a bin takes
array[1..num_bins] of var 0..bin_capacity: bin_loads;

% number of loaded bins (which we will minimize)
var 0..num_bins: num_loaded_bins;

% minimize the number of loaded bins
% solve minimize num_loaded_bins;

% alternative solve statement
solve :: int_search(
         [bins[i,j] | i in 1..num_bins, j in 1..num_stuff] 
         % ++ bin_loads 
         , 
         input_order, % first_fail, 
         indomain_max, 
         complete) 
     minimize num_loaded_bins;

constraint

    % sanity clause: No thing can be larger than capacity.
    % forall(s in 1..num_stuff) ( 
    %    stuff[s] <= bin_capacity 
    % )

    % /\ % the total load in the bin cannot exceed bin_capacity
    forall(b in 1..num_bins) (
       bin_loads[b] = sum(s in 1..num_stuff) (stuff[s]*bins[b,s]) 
    ) 

    /\ % calculate the total load for a bin
    sum(s in 1..num_stuff) (stuff[s]) = sum(b in 1..num_bins) (bin_loads[b])

    /\ % a thing is packed just once 
    forall(s in 1..num_stuff) ( 
       sum(b in 1..num_bins) (bins[b,s])  = 1 
    )

    % /\ % symmetry breaking: 
    %    % if bin_loads[i+1] is > 0 then bin_loads[i] must be > 0
    %  forall(b in 1..num_bins-1) (
    %    (bin_loads[b+1] > 0 -> bin_loads[b] > 0)
    %    % /\ % and should be filled in order of weight
    %    % bin_loads[b] >= bin_loads[b+1] 
    %  ) 
    /\
    decreasing(bin_loads) :: domain
    /\ % another symmetry breaking: first bin must be loaded
    bin_loads[1] > 0 

    /\ % calculate num_loaded_bins
    num_loaded_bins = sum(b in 1..num_bins) (bool2int(bin_loads[b] > 0))

;

%
% Output
%
output [
       if (b = 1 /\ s = 1) then "num_loaded_bins: " ++ show(num_loaded_bins) ++ "\n" else "" endif ++
       if s = 1 then show(bin_loads[b]) ++ " : " else "" endif ++
           show(bins[b,s]) ++ 
           if s = num_stuff then "\n" else " " endif
       | b in 1..num_bins, s in 1..num_stuff
];


%
% data
%

% simple (and probably unrealistic) packing
% num_stuff = 20;
% stuff = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];
% bin_capacity = 30; 

% The problem below is from
% http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
% 1D Bin Packing (or "CP? Who cares?"), page 3
% and also from
% http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
%
% num_stuff = 10;
% num_bins = num_stuff;
% stuff = [42,63,67,57,93,90,38,36,45,42];
% bin_capacity = 150;


% This problem instance is via
% http://yetanothermathprogrammingconsultant.blogspot.com/2011/08/bin-packing.html
% and is "critical" to the result in
% "Bin Packing, What is It?"
% http://www.developerfusion.com/article/5540/bin-packing/
% where the minimum bins are 19,
% whereas Erwin K's code have 17 as the optimal result.
% 
% LazyFD solves this in 1:44 minutes (i.e. with result 17) 
% when num_bins is hardcoded to 19 (as in Erwin's model)
% 
num_stuff = 45;
num_bins = 19;
stuff = [
 26,   57, 18,   8,  45,
 16,   22, 29,   5,  11,
  8,   27, 54,  13,  17,
 21,   63, 14,  16,  45,
  6,   32, 57,  24,  18,
 27,   54, 35,  12,  43,
 36,   72, 14,  28,   3,
 11,   46, 27,  42,  59,
 26,   41, 15,  41,  68];
bin_capacity = 80;



% same source of data, but now there are 22 things
% num_stuff = 22; % number of things
% num_bins = num_stuff;
% stuff = [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,85,33];
% bin_capacity = 250;

%
% still more stuff
%
% num_stuff = 50;
% num_bins = num_stuff;
% stuff = [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,41,49,55,98,80,32,25,38,82,30]; % ,35,39,57,84,62,50,55,27,30,36,20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,3];
% bin_capacity = 290;

% Example from the Alice system
% Copying files to disks
% http://news.mozart-oz.org/alice/manual/cptutorial/node55.html
%
% """
% Suppose, you want to copy a set of files from your hard-disk onto as 
% few as possible diskettes of a given size, e. g. onto common 1.44 MB 
% diskettes. In case your files do not fit on a single diskette, it might 
% become quite tricky to figure out the minimal number of needed diskettes 
% and how to partition the files.
% """
% num_stuff = 6;
% num_bins = num_stuff;
% stuff = [360, 850, 630, 70, 700, 210]; % size of files
% bin_capacity = 1440; % size of a floppy
