/*

  Bin packing in SICStus Prolog.

  Simple bin packing problems

  Compare to the following models
  * MiniZinc: http://www.hakank.org/minizinc/bin_packing.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/bin_packing.ecl


  This model is based on the ECLiPSe model but has been
  completely rewritten. 

  Mats Carlsson suggested the solution in bin_packing2/1.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(samsort)).


% sorted Stuff
go :-
        ( for(P,0,9) do
              bin_packing(P,sorted)
        ).


% Unsorted Stuff: very slow for problem 8 and 9
go2 :-
        ( for(P,0,9) do
              bin_packing(P,unsorted)
        ).

% Mats Carlsson's variant with unsorted 
go3 :-
        ( for(P,0,9) do
              bin_packing2(P)
        ).


bin_packing(Problem, SortMode) :-

        problem(Problem, StuffUnordered, BinCapacity),

        % sorting (reversed) the stuff makes it much faster
        % (but maybe this is considered as cheating)
        (
            SortMode = sorted 
        -> 
            samsort(#>=,StuffUnordered,Stuff)
        ;
            Stuff = StuffUnordered
        ),

        format('\nProblem ~w\n',[Problem]),

        % BinCapacity: the (common) capacity for each bin
        write(bin_capacity:BinCapacity),nl,

        % Stuff: values/weight of the things to pack
        length(Stuff,NumStuff), 
        write(stuff:Stuff),nl,
        write(num_stuff:NumStuff),nl,

        % Number of bins cannot exceed num_stuff...
        NumBins = NumStuff,

        % Sanity clause: 
        %    No thing can be larger than capacity.
        ( foreach(S,Stuff),
          param(BinCapacity) do
              S > BinCapacity 
        -> 
          format('Stuff ~d is larger than BinCapacity ~d\n', [S, BinCapacity])
        ;
          true
        ),
        
        % Bins: where to put a thing.
        matrix(Bins,[NumBins,NumStuff]),
        append(Bins,BinsList),
        domain(BinsList,0,1),        

        % BinLoads: contains how many things (the summed 
        %           weights of) a bin takes
        length(BinLoads,NumBins),
        domain(BinLoads,0,BinCapacity),
        ( foreach(Bin,Bins),
          foreach(Load,BinLoads),
          param(Stuff) do
              scalar_product(Stuff,Bin,#=,Load)
        ),
       
        % a thing is packed exactly once 
        transpose(Bins,BinsTransposed),
        ( foreach(Column, BinsTransposed) 
        do
            sum(Column,#=, 1)
        ),

        % compare the total loads
        sum(Stuff,#=,StuffSum),
        sum(BinLoads,#=,BinLoadsSum),
        StuffSum #= BinLoadsSum,

        % load the bins in order:
        % first bin must be loaded, and the list must be ordered
        element(1, BinLoads, BinLoads1),
        BinLoads1 #> 0,
        ( fromto(BinLoads,[This,Next|Rest],[Next|Rest],[_])
        do
          This #>= Next
        ),

        % calculate number of loaded bins 
        % (which we will minimize)
        % domain([NumLoadedBins],0,1000000),
        ( foreach(Load2,BinLoads),
          fromto(0,In,Out,NumLoadedBins),
          param(BinCapacity)
        do
          Loaded in 0..1,
          Load2 #> 0 #<=> Loaded #= 1,
          indomain(Loaded),
          Out #= In + Loaded
        ),

        % search
        append(BinsList,BinLoads,Vars),
        % append([NumLoadedBins],Vars1,Vars),
        labeling([ff,step,down,minimize(NumLoadedBins)], Vars),

        % output
        write(bin_loads:BinLoads),nl,
        write(num_loaded_bins:NumLoadedBins),nl,

        % just print smaller problems
        (NumBins =< 40 
        -> 
         % pretty_print(Bins) 
         pretty_print2(Bins) 
        ; 
         write('Bins is too large to print.\n'), true
        ),
        fd_statistics,
        nl.

% This version using cumulative was suggested by Mats Carlsson.
bin_packing2(Problem) :-
        problem(Problem, StuffUnordered, BinCapacity),
        portray_clause(problem(Problem, StuffUnordered, BinCapacity)),
        length(StuffUnordered, N),
        N1 is N+1,
        (   foreach(H,StuffUnordered),
            foreach(task(S,1,E,H,1),Tasks),
            foreach(NH-S,Tagged),
            foreach(S,Assignment),
            foreach(_-X,Keysorted),
            foreach(X,Vars),
            param(N1)
        do  S in 1..N1,
            E in 2..N1,
            NH is -H
        ),
        keysort(Tagged, Keysorted),
        maximum(Max, Vars),
        cumulative(Tasks, [limit(BinCapacity)]),
        labeling([minimize(Max),step], Vars),
        portray_clause(assignment(Assignment)),
        fd_statistics,
        nl.



%
% pretty prints the Bin matrix.

pretty_print(X) :-
        ( foreach(Row,X) 
        do
          sum(Row,#=,Sum),
          ( foreach(R,Row) do
                format('~d ', R)
          ),
          format(" = ~d\n", [Sum])
        ).

% just print the filled bins
pretty_print2(X) :-
        ( foreach(Row,X) 
        do
          sum(Row,#=,Sum),
          Sum > 0 -> 
              (
                  ( foreach(R,Row) do
                        format('~d ', R)
                  ),
                  format(" = ~d\n", [Sum])
              )
        ;
              true
        ).


% Suggested by Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


%
% Data
%        

% easy problem to test with
problem(0, 
        [1,2,3,4,5,6,7,8,9,10],
        30).


%
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
problem(1, 
        [360, 850, 630, 70, 700, 210], 
        1440).


% simple (and probably unrealistic) packing
problem(2, 
        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
        20).


% simple (and probably even less unrealistic) packing
problem(3, 
        [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],
        50).


% This problem below is from
% http://www.dcs.gla.ac.uk/~pat/cpM/slides/binPacking.ppt
% 1D Bin Packing (or "CP? Who cares?"), page 3
% and also from
% http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/binPack
%
% num_stuff = 10;
% stuff = [42,63,67,57,93,90,38,36,45,42];
% bin_capacity = 150;
problem(4, 
        [42,69,67,57,93,90,38,36,45,42], 
        150).


% same source of data, but now there are 22 things
problem(5, 
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33],
        250).


%
% continuing of the above example.
%
problem(6, 
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,
         46,38,85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,
         30,64,29,74,41,49,55,98,80,32,25,38,82,30], 
        290).


% ibid. 
problem(7,
        [42,69,67,57,93,90,38,36,45,42,33,79,27,57,44,84,86,92,46,38,
         85,33,82,73,49,70,59,23,57,72,74,69,33,42,28,46,30,64,29,74,
         41,49,55,98,80,32,25,38,82,30,35,39,57,84,62,50,55,27,30,36,
         20,78,47,26,45,41,58,98,91,96,73,84,37,93,91,43,73,85,81,79,
         71,80,76,83,41,78,70,23,42,87,43,84,60,55,49,78,73,62,36,44,
         94,69,32,96,70,84,58,78,25,80,58,66,83,24,98,60,42,43,43,
         39],
        500).


% From 
% Graham Kendall: Bin Packing made Easier 
% http://research-reflections.blogspot.com/2009/07/bin-packing-made-easier.html
problem(8,
        [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
        252,252,127,85,12,10,9,252,127,106,84,12,10,252,
        127,106,46,12,10],
        524).


% Variant: remove 46 from the problem above
problem(9,
        [442,252,127,106,37,10,10,252,252,127,106,37,10,9,
        252,252,127,85,12,10,9,252,127,106,84,12,10,252,
        127,106,12,10],
        524).
